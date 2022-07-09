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

thing : HasIO io => io Int -> String -> io Int
thing i a = do
  i <- i
  _ <- primIO $ prim__setFewArg i a
  pure $ i + 1

||| Fork, exec, and wait.
few : HasIO io => List String -> io (Either (Maybe FileError) Int)
few cmd = do
  _ <- foldl thing (pure 0) cmd
  res <- primIO $ prim__few
  if res < 0
    then case res of
      -2 => returnMaybeError
      _ => pure $ Left Nothing
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

transposeIO : HasIO io => List (io a) -> io (List a)
transposeIO (x :: xs) = pure (!x :: !(transposeIO xs))
transposeIO [] = pure []

transposeEither : List (Either a b) -> Either a (List b)
transposeEither (x :: xs) = case x of
  Right y => case transposeEither xs of
    Right ys => Right $ y :: ys
    Left es => Left es
  Left e => Left e
transposeEither [] = Right []

linkJoin : HasIO io => String -> String -> String -> io (Either FileError ())
linkJoin src dest name = symlink (src ++ "/" ++ name) (dest ++ "/" ++ name)

sanitizePathPart : HasIO io => String -> List String -> String -> io (Either FileError String)
sanitizePathPart tmp ex p = case !(listDir p) of
  -- if a path section can't be read, leave it untouched
  Left _ => pure $ Right p
  Right con => case intersect ex con of
    [] => pure $ Right p -- then there are no unwanted binaries in this path
    _ => case !(createTmpDir $ tmp ++ "/px-") of
      Left e => pure $ Left e
      Right tmp => pure $ case transposeEither !(transposeIO $ map (linkJoin p tmp) $ con \\ ex) of
        Left e => Left e
        Right _ => Right tmp

removeAll : HasIO io => String -> io (Either FileError ())
removeAll s = removeFile s

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

main : IO ()
main = do
  -- resolve the temporary directory
  -- TODO: ensure tmp is removed in all cases
  let tmp = case find isJust !(transposeIO $ map getEnv ["TMP", "TEMP", "TMPDIR", "TEMPDIR"]) of
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
    _ => case transposeEither !(transposeIO $ map (sanitizePathPart tmp ex) path) of
      Left e => logErr "while preparing new $PATH" $ Just e
      Right newPath => case !(setEnv "PATH" (concat $ intersperse ":" newPath) True) of
        False => logErr "failed to set new $PATH" Nothing
        True => case !(few cmd) of
          Left e => logErr "failed toe execute command" e
          Right ec => case ec of
            0 => exitWith ExitSuccess
            nz => exitWith $ ExitFailure 1 -- TODO: figure out how to provide proof that nz is non-zero
