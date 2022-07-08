module PathExclude.Main

-- TODO: clean up imports
import Data.Fin
import Data.List
import Data.List1
import Data.String
import Data.Maybe
import System
import System.Directory
import System.File
import System.Random

%foreign "C:symlink,libc 6"
prim__symlink : String -> String -> PrimIO Int

-- TODO: don't ignore errors here
||| Make a new name for a file
|||
||| @ t the target path the link will point to
||| @ l the linkpath the link will be created at
symlink : HasIO io => (t : String) -> (l : String) -> io ()
symlink t l = ignore $ primIO (prim__symlink t l)

-- TODO: don't hardcode this, and figure out why big numbers make the compiler
-- slow, it probably has something to do with having to construct the Fin type,
-- maybe I can use something other than rndFin?
rndMax : Nat
rndMax = 9999

createTmpDirRetrying : HasIO io => String -> Nat -> io (Either FileError String)
createTmpDirRetrying p (S k) = do
  let t = p ++ show !(rndFin rndMax)
  case !(createDir t) of
    Left e => createTmpDirRetrying p k
    Right _ => pure $ Right t
createTmpDirRetrying p Z = do
  let t = p ++ show !(rndFin rndMax)
  case !(createDir t) of
    Left e => pure $ Left e
    Right _ => pure $ Right t

createTmpDir : HasIO io => String -> io (Either FileError String)
createTmpDir p = createTmpDirRetrying p 10000

-- TODO: figure out how to unify the following two into one, or find a
-- base/prelude function that already solves this

transposeIO : List (IO a) -> IO (List a)
transposeIO (x :: xs) = pure (!x :: !(transposeIO xs))
transposeIO [] = pure []

transposeIO' : HasIO io => List (io a) -> io (List a)
transposeIO' (x :: xs) = pure (!x :: !(transposeIO' xs))
transposeIO' [] = pure []

transposeEither : List (Either a b) -> Either a (List b)
transposeEither (x :: xs) = case x of
  Right y => case transposeEither xs of
    Right ys => Right $ y :: ys
    Left es => Left es
  Left e => Left e
transposeEither [] = Right []

linkJoin : HasIO io => String -> String -> String -> io ()
linkJoin src dest name = symlink (src ++ "/" ++ name) (dest ++ "/" ++ name)

sanitizePathPart : HasIO io => String -> List String -> String -> io (Either FileError String)
sanitizePathPart tmp ex p = case !(listDir p) of
  -- if a path section can't be read, leave it untouched
  Left _ => pure $ Right p
  Right con => case intersect ex con of
    [] => pure $ Right p -- then there are no unwanted binaries in this path
    _ => case !(createTmpDir $ tmp ++ "/px-") of
      Left e => pure $ Left e
      Right tmp => do
        _ <- transposeIO' $ map (linkJoin p tmp) $ con \\ ex
        pure $ Right tmp

main : IO ()
main = do
  -- resolve the temporary directory
  -- TODO: ensure tmp is removed in all cases
  let tmp = case find (isJust) !(transposeIO $ map getEnv ["TMP", "TEMP", "TMPDIR", "TEMPDIR"]) of
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
    [] => do
      _ <- fPutStrLn stderr "px: no command provided, separate executables from command with `--`"
      pure ()
    _ => case transposeEither !(transposeIO $ map (sanitizePathPart tmp ex) path) of
      Left e => do
        _ <- fPutStrLn stderr $ "px: while preparing new $PATH: " ++ show e
        pure ()
      Right newPath => case !(setEnv "PATH" (concat $ intersperse ":" newPath) True) of
        False => do
          _ <- fPutStrLn stderr "px: failed to set new $PATH"
          pure ()
        True => do
          -- TODO: properly pipe all std{out,err,in}
          (s, ec) <- run cmd
          _ <- putStr s
          case ec of
            0 => exitWith ExitSuccess
            nz => exitWith $ ExitFailure 1 -- TODO: figure out how to provide proof that nz is non-zero
