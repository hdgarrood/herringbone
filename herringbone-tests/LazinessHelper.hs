module LazinessHelper where

import Web.Herringbone
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import System.Process.ByteString (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Test.Hspec (pendingWith)

es :: FilePath -> String
es = F.encodeString

lp :: Text -> LogicalPath
lp = unsafeMakeLogicalPath . T.splitOn "/"

cleanDir :: FilePath -> IO ()
cleanDir d = F.removeTree d >> F.createTree d

-- | Does a program exist on the PATH?
which :: String -> IO Bool
which prog = do
    (code, _, _) <- readProcessWithExitCode "which" [prog] ""
    return $ case code of
        ExitSuccess -> True
        _ -> False

requiresExecutable :: String -> IO ()
requiresExecutable prog = do
    exists <- which prog
    when (not exists) (pendingWith ("missing executable: " ++ prog))

-- | Return the absolute paths of all files (excluding directories and other
-- things) below the given root.
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive root = do
    results         <- F.listDirectory root
    (files, others) <- partitionM F.isFile results
    (dirs, _)       <- partitionM F.isDirectory others
    subfiles        <- mapM getFilesRecursive dirs
    return $ files ++ concat subfiles

-- | Return the relative paths of all files (excluding directories and other
-- things) below the given root.
getFilesRecursiveRelative :: FilePath -> IO [FilePath]
getFilesRecursiveRelative root = do
    root' <- F.canonicalizePath (root </> "") -- this is required, because. :/
    files <- getFilesRecursive root'
    let maybes = map (F.stripPrefix root') files
    return $ catMaybes maybes
