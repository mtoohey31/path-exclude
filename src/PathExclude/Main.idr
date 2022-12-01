module PathExclude.Main

import Data.List1
import Data.String
import System
import System.Directory

%foreign "C:symlink,libc,unistd.h"
prim__symlink : String -> String -> PrimIO Int
%foreign "C:setenv,libc,stdlib.h"
prim__setenv : String -> String -> Int -> PrimIO Int
%foreign "C:rand,libc,stdlib.h"
prim__random : PrimIO Int
%foreign "C:idris2_setFewArgc,libidris2_few,idris_few.h"
prim__setFewArgc : Int -> PrimIO ()
%foreign "C:idris2_setFewArgn,libidris2_few,idris_few.h"
prim__setFewArgn : Int -> String -> PrimIO ()
%foreign "C:idris2_few,libidris2_few,idris_few.h"
prim__few : PrimIO Int

||| Make a new name for a file.
|||
||| @ t the target path the link will point to
||| @ l the linkpath the link will be created at
symlink : HasIO io => (t : String) -> (l : String) -> io (Either FileError ())
symlink t l = do
  res <- primIO $ prim__symlink t l
  if res == 0
    then pure $ Right ()
    else returnError

||| Set the specified environment variable.
|||
||| @ var the name of the environment variable to set
||| @ val the value to set the variable to
setEnv' : HasIO io => (var : String) -> (val : String) -> io (Either FileError ())
setEnv' var val = do
  res <- primIO $ prim__setenv var val 1
  if res == 0
    then pure $ Right ()
    else returnError

||| Get a random number.
random : HasIO io => io Int
random = primIO prim__random

||| Return the error indicated by errno as `Left $ Just e` if it exists,
||| instead of as `Left e` as `returnError` does.
returnMaybeError : HasIO io => io (Either (Maybe FileError) a)
returnMaybeError = case !returnError of
  Left e => pure $ Left $ Just e
  Right a => pure $ Right a

||| Set an argument to be used by `few`.
setFewArgn : HasIO io => io Int -> String -> io Int
setFewArgn i a = do
  _ <- primIO $ prim__setFewArgn !i a
  pure $ !i + 1

||| Fork, exec, and wait.
few : HasIO io => List String -> io (Either (Maybe FileError) Int)
few cmd = do
  _ <- primIO $ prim__setFewArgc $ cast $ length cmd
  _ <- foldl setFewArgn (pure 0) cmd
  res <- primIO $ prim__few
  if res < 0
    then if res == -2 then returnMaybeError else pure $ Left Nothing
    else pure $ Right res

||| Transform a list of monad values to a list value of that same monad.
batchM : Monad m => List (m a) -> m (List a)
batchM (x :: xs) = pure (!x :: !(batchM xs))
batchM [] = pure []

||| Transform a list of lazy, maybe monad values to a value of that same
||| monad the first just value that was produced occurred, or nothing if no
||| values were produced.
|||
||| A lazy input type allows us to ensure that no further operations are
||| performed after the first success.
batchOM : Monad m => List (Lazy (m (Maybe a))) -> m (Maybe a)
batchOM (x :: xs) = case !x of
  Just j => pure $ Just j
  Nothing => batchOM xs
batchOM [] = pure Nothing

||| Transform a list of lazy, fallible monad values to a value of that same
||| monad containing either the first error that occurred, or the list of
||| values that were produced if no errors occurred.
|||
||| A lazy input type allows us to ensure that no further operations are
||| performed after the first failure.
batchFM : Monad m => List (Lazy (m (Either a b))) -> m (Either a (List b))
batchFM (x :: xs) = case !x of
  Left e => pure $ Left e
  Right y => case !(batchFM xs) of
    Left e => pure $ Left e
    Right ys => pure $ Right $ y :: ys
batchFM [] = pure $ Right []

||| Create a temporary directory with the given prefix, retrying the given
||| amount of times if a failure occurs.
createTmpDirRetrying : HasIO io => String -> Nat -> io (Either FileError String)
createTmpDirRetrying p (S k) = do
  let t = p ++ show !random
  case !(createDir t) of
    Left e => createTmpDirRetrying p k
    Right _ => pure $ Right t
createTmpDirRetrying p Z = do
  let t = p ++ show !random
  case !(createDir t) of
    Left e => pure $ Left e
    Right _ => pure $ Right t

||| Create a temporary directory, retrying the default amount of times.
createTmpDir : HasIO io => String -> io (Either FileError String)
createTmpDir p = createTmpDirRetrying p 10000

||| Transform an either whose left and right sides are of the same type to the
||| inner value, regardless of its side.
unify : Either a a -> a
unify e = case e of
  Left e => e
  Right e => e

||| Get all left elements of a list of either values.
getLefts : List (Either a b) -> List a
getLefts (x :: xs) = case x of
  Left y => y :: (getLefts xs)
  Right _ => getLefts xs
getLefts [] = []

||| Create a symlink from `dest ++ "/" ++ name` to `src ++ "/" ++ name`.
linkJoin : HasIO io => String -> String -> String -> io (Either FileError ())
linkJoin src dest name = symlink (src ++ "/" ++ name) (dest ++ "/" ++ name)

||| If a `$PATH` entry contains any of the binaries to be excluded, return the
||| path of a temporary directory to be used in the entry's place that excludes
||| the unwanted binaries. Otherwise, return the entry unchanged.
cleanEntry : HasIO io => String -> List String -> String -> io (Either FileError (Either String String))
cleanEntry tmp ex p = case !(listDir p) of
  -- if a path section can't be read, leave it untouched
  Left _ => pure $ Right $ Right p
  Right con => case intersect ex con of
    [] => pure $ Right $ Right p -- then there are no unwanted binaries in this path
    _ => case !(createTmpDir $ tmp ++ "/px-") of
      Left e => pure $ Left e
      Right tmp => pure $ case !(batchFM $ map (delay . (linkJoin p tmp)) $ con \\ ex) of
        Left e => Left e
        Right _ => Right $ Left tmp

||| Log an error.
|||
||| Ignores errors when writing the error, because what are we going to do, try
||| to print the error to the same plae that we just failed to write to?
logErr : HasIO io => String -> Maybe FileError -> io ()
logErr s e = case e of
  Just e => do
    _ <- fPutStrLn stderr $ "px: " ++ s ++ ": " ++ show e
    pure ()
  Nothing => do
    _ <- fPutStrLn stderr $ "px: " ++ s
    pure ()

||| Remove a directory and all entries inside it.
|||
||| Operates non-recursively, because everything in a temporary path directory
||| that we're removing should be a symlink, so no recursion is necessary even
||| if there are symlinks to directories.
removeAll : HasIO io => String -> io (Either FileError ())
removeAll p = case !(listDir p) of
  Left e => pure $ Left e
  Right con => case !(batchFM $ map (delay . removeFile) $ map (p ++ "/" ++) con) of
    Left e => pure $ Left e
    Right _ => removeFile p

||| Log the given error and exit with code 1.
dieErr : HasIO io => String -> Maybe FileError -> io ()
dieErr s e = do
  _ <- logErr s e
  exitWith $ ExitFailure 1

||| Remove the temporary directories, log the given error, and exit with code
||| 1.
dieErrAndRm : List String -> (HasIO io => String -> Maybe FileError -> io ())
dieErrAndRm p s e = do
  _ <- case !(batchFM $ map (delay . removeAll) p) of
    Left re => logErr "while removing a temporary directory" $ Just re
    Right _ => pure ()
  logErr s e

main : IO ()
main = do
  -- resolve the temporary directory
  let tmp = case !(batchOM $ map (delay . getEnv) ["TMP", "TEMP", "TMPDIR", "TEMPDIR"]) of
        Just t => t
        _ => "/tmp"
  
  -- fetch the sections of $PATH
  path <- case !(getEnv "PATH") of
    Just s => pure $ forget $ split (== ':') s
    Nothing => pure [] -- if path is unset, treat it as empty

  -- get args and split on "--"
  let (ex, cmd) = span (not . (== "--")) $ drop 1 !getArgs

  let cmd = drop 1 cmd -- drop the "--"

  case cmd of
    [] => logErr "no command provided, separate executables from command with `--`" Nothing
    _ => case !(batchFM $ map (delay . (cleanEntry tmp ex)) path) of
      Left e => logErr "while preparing new $PATH" $ Just e
      Right items => do
        let toRm = getLefts items
        let dieRm = dieErrAndRm toRm
        let newPath = map unify items
        case !(setEnv' "PATH" (concat $ intersperse ":" newPath)) of
          Left e => dieRm "failed to set new $PATH" $ Just e
          Right () => case !(few cmd) of
            Left me => case me of
              Just e => dieRm "failed to execute command" $ Just e
              Nothing => dieRm "failed to execute command: unknown error" Nothing
            Right ec => do
              _ <- case !(batchFM $ map (delay . removeAll) toRm) of
                Left e => logErr "while removing a temporary directory" $ Just e
                Right _ => pure ()
              case choose $ ec == 0 of
                Left _ => exitWith ExitSuccess
                Right _ => exitWith $ ExitFailure ec
