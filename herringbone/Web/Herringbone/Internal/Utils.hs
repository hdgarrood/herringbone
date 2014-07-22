module Web.Herringbone.Internal.Utils where

import Control.Monad
import Data.Maybe (catMaybes)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import Prelude hiding (FilePath)

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

-- should this go in a utils module?
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = foldM g ([], [])
    where
    g (as, bs) x = do
        flag <- f x
        return $ if flag
            then (x : as, bs)
            else (as, x : bs)
