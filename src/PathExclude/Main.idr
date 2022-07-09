module PathExclude.Main

import Data.Fin
import Data.IOArray
import Data.List
import Data.List1
import Data.String
import System
import System.Directory
import System.Random

%foreign "C:symlink,libc 6"
prim__symlink : String -> String -> PrimIO Int
%foreign "C:rand,libc 6"
prim__random : PrimIO Int
%foreign "C:idris2_few, libidris2_few, idris_few.h"
prim__few : PrimIO Int
%foreign "C:idris2_setFewArg, libidris2_few, idris_few.h"
prim__setFewArg : Int -> String -> PrimIO Int

||| Make a new name for a file
|||
||| @ t the target path the link will point to
||| @ l the linkpath the link will be created at
symlink : HasIO io => (t : String) -> (l : String) -> io (Either FileError ())
symlink t l = do
  res <- primIO $ prim__symlink t l
  if res == 0
    then pure $ Right ()
    else returnError

||| Get a random number.
random : HasIO io => io Int
random = primIO prim__random

returnMaybeError : HasIO io => io (Either (Maybe FileError) a)
returnMaybeError = case !returnError of
  Left e => pure $ Left $ Just e
  Right a => pure $ Right a

setFewArg : HasIO io => io Int -> String -> io Int
setFewArg i a = do
  _ <- primIO $ prim__setFewArg !i a
  pure $ !i + 1

||| Fork, exec, and wait.
few : HasIO io => List String -> io (Either (Maybe FileError) Int)
few cmd = do
  _ <- foldl setFewArg (pure 0) cmd
  res <- primIO $ prim__few
  if res < 0
    then if res == -2 then returnMaybeError else pure $ Left Nothing
    else pure $ Right res

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

createTmpDir : HasIO io => String -> io (Either FileError String)
createTmpDir p = createTmpDirRetrying p 10000

tpM : Monad m => List (m a) -> m (List a)
tpM (x :: xs) = pure (!x :: !(tpM xs))
tpM [] = pure []

tpE : List (Either a b) -> Either a (List b)
tpE (x :: xs) = case x of
  Right y => case tpE xs of
    Right ys => Right $ y :: ys
    Left es => Left es
  Left e => Left e
tpE [] = Right []

unify : Either a a -> a
unify e = case e of
  Left e => e
  Right e => e

getLefts : List (Either a b) -> List a
getLefts (x :: xs) = case x of
  Left y => y :: (getLefts xs)
  Right _ => getLefts xs
getLefts [] = []

linkJoin : HasIO io => String -> String -> String -> io (Either FileError ())
linkJoin src dest name = symlink (src ++ "/" ++ name) (dest ++ "/" ++ name)

cleanEntry : HasIO io => String -> List String -> String -> io (Either FileError (Either String String))
cleanEntry tmp ex p = case !(listDir p) of
  -- if a path section can't be read, leave it untouched
  Left _ => pure $ Right $ Right p
  Right con => case intersect ex con of
    [] => pure $ Right $ Right p -- then there are no unwanted binaries in this path
    _ => case !(createTmpDir $ tmp ++ "/px-") of
      Left e => pure $ Left e
      Right tmp => pure $ case tpE !(tpM $ map (linkJoin p tmp) $ con \\ ex) of
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

dieErr : HasIO io => String -> Maybe FileError -> io ()
dieErr s e = do
  _ <- logErr s e
  exitWith $ ExitFailure 1

||| Remove a directory recursively
removeAll : HasIO io => String -> io (Either FileError ())
removeAll p = case !(listDir p) of
  Left e => pure $ Left e
  Right con => case tpE $ !(tpM $ map removeFile $ map (p ++ "/" ++) con) of
    Left e => pure $ Left e
    Right _ => removeFile p

dieErrAndRm : List String -> (HasIO io => String -> Maybe FileError -> io ())
dieErrAndRm p s e = do
  _ <- case tpE !(tpM $ map removeAll p) of
    Left re => logErr "while removing a temporary directory" $ Just re
    Right _ => pure ()
  logErr s e

main : IO ()
main = do
  -- resolve the temporary directory
  let tmp = case find isJust !(tpM $ map getEnv ["TMP", "TEMP", "TMPDIR", "TEMPDIR"]) of
        Just (Just t) => t
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
    _ => case tpE !(tpM $ map (cleanEntry tmp ex) path) of
      Left e => logErr "while preparing new $PATH" $ Just e
      Right items => do
        let toRm = getLefts items
        let dieRm = dieErrAndRm toRm
        let newPath = map unify items
        case !(setEnv "PATH" (concat $ intersperse ":" newPath) True) of
          False => dieRm "failed to set new $PATH" Nothing
          True => case !(few cmd) of
            Left me => case me of
              Just e => dieRm "failed to execute command" $ Just e
              Nothing => dieRm "failed to execute command: unknown error" Nothing
            Right ec => do
              _ <- case tpE !(tpM $ map removeAll toRm) of
                Left e => logErr "while removing a temporary directory" $ Just e
                Right _ => pure ()
              case choose $ ec == 0 of
                Left _ => exitWith ExitSuccess
                Right _ => exitWith $ ExitFailure ec
