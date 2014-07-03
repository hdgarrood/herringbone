-- | This module deals with locating assets on the disk, and determining which
-- assets needs preprocessing.
--
-- In development mode:
-- * At startup, build a mapping of source files to destination files together
--   with any preprocessors that should be run on them (based on extension)
-- * watch for filesystem changes, and rebuild relevant parts of this mapping
--   when necessary
-- * listen for HTTP requests and serve relevant files, performing
--   preprocessing where necessary.
-- (well, eventually do all that. For now just rebuild the BuildMapping for
-- each request).
--
-- In production mode:
-- * build the mapping
-- * preprocess all the files and output them to a particular directory.
--
-- This architecture should ensure that the file mapping is identical in each
-- mode.
--
module Web.Herringbone.GetBuildMapping where

import Control.Monad
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import Prelude hiding (FilePath)

import Web.Herringbone.Types

getBuildMapping :: Herringbone -> IO BuildMapping
getBuildMapping hb = do
    files <- getFilesRecursiveRelative $ hbSourceDir hb
    return $ map (getBuildSpec hb) files

-- | Return the absolute paths of all files (excluding directories and other
-- things) below the given root.
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive root = do
    results         <- F.listDirectory root
    (files, others) <- partitionM F.isFile results
    (dirs, _)       <- partitionM F.isDirectory others
    subfiles        <- sequence $ map getFilesRecursive dirs
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
partitionM f list = foldM g ([], []) list
    where
    g (as, bs) x = do
        flag <- f x
        if flag
            then return (x : as, bs)
            else return (as, x : bs)

-- | Given a FilePath of a source file, construct a BuildSpec for the file.
getBuildSpec :: Herringbone -> FilePath -> BuildSpec
getBuildSpec hb sourcePath = BuildSpec sourcePath destPath pp
    where
    (destPath, pp) =
        fromMaybe (sourcePath, Nothing) $ do
            extension <- F.extension sourcePath
            pp'       <- lookupPP extension (hbPPs hb)
            destPath' <- swapExtension pp' sourcePath
            return (destPath', Just pp')

-- | Make the destination path of a BuildSpec absolute, using the destination
-- directory of the given Herringbone.
makeDestAbsolute :: Herringbone
                 -> BuildSpec
                 -> IO BuildSpec
makeDestAbsolute hb (BuildSpec sourcePath destPath pp) = do
    fullDestDir <- F.canonicalizePath $ hbDestDir hb
    let fullDestPath = fullDestDir </> destPath
    return $ BuildSpec sourcePath fullDestPath pp

swapExtension :: PP -> FilePath -> Maybe FilePath
swapExtension pp =
    swapExtension' (ppConsumes pp) (ppProduces pp)

swapExtension' :: Text -> Text -> FilePath -> Maybe FilePath
swapExtension' fromExt toExt path = do
    guard $ F.hasExtension path fromExt
    return $ F.replaceExtension path toExt

-- | Search for a file in a list of search paths. For example, if
-- @assets/test.txt@ exists, then
-- @searchForFile ["assets", "other_assets"] "test.txt"@ will return
-- @Just "assets/text.txt"@
searchForFile :: [FilePath]     -- ^ List of search paths
              -> FilePath       -- ^ File to search for
              -> IO (Maybe FilePath)
searchForFile searchPath path = do
    let fullPaths = map (\x -> x </> path) searchPath
    matches <- filterM F.isFile fullPaths
    case matches of
        []    -> return Nothing
        (x:_) -> fmap Just $ F.canonicalizePath x
