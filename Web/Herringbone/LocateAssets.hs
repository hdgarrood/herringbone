-- | This module deals with locating assets on the disk, and calculating how to
-- create assets which need preprocessing.
module Web.Herringbone.LocateAssets where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import Prelude hiding (FilePath)

import Web.Herringbone.Types

-- | Calculates a list of BuildSpecs that could be used to build the asset with
-- the given logical path.
locateAssets :: Herringbone -> LogicalPath -> IO [BuildSpec]
locateAssets hb =
    undefined

-- do
--     specs <- getBuildSpecsBackwards hb logPath
--     fullSpecs <- sequence $ map (locateFiles hb) specs
--     return $ catMaybes fullSpecs

-- | Calculates a list of BuildSpecs that could be used to build the asset with
-- the given source path.
locateAssetsReverse :: Herringbone -> FilePath -> IO (Maybe BuildSpec)
locateAssetsReverse hb sourcePath =
    return spec
        >>= makeDestAbsolute hb
        >>= locateSource hb
    where
    spec = getBuildSpecForwards hb sourcePath

getBuildSpecsBackwards :: Herringbone
                       -> LogicalPath
                       -> [BuildSpec]
getBuildSpecsBackwards hb path = do
    undefined

getBuildSpecForwards :: Herringbone
                     -> FilePath
                     -> BuildSpec
getBuildSpecForwards hb sourcePath = BuildSpec sourcePath destPath pp
    where
    (destPath, pp) =
        fromMaybe (sourcePath, Nothing) $ do
            extension <- F.extension sourcePath
            pp' <- lookupConsumer (hbPPs hb) extension
            destPath' <- swapExtensionForwards pp' sourcePath
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

-- | Search through the Herringbone's source paths to find the absolute path of
-- the referenced source file.
locateSource :: Herringbone
             -> BuildSpec
             -> IO (Maybe BuildSpec)
locateSource hb (BuildSpec sourcePath destPath pp) = do
    fullSourcePath <- searchForFile (hbSourceDirs hb) sourcePath
    return $ fmap (\sp -> BuildSpec sp destPath pp) fullSourcePath

swapExtensionBackwards :: PP -> FilePath -> Maybe FilePath
swapExtensionBackwards pp =
    swapExtension
        (ppProduces . ppSpec $ pp)
        (ppConsumes . ppSpec $ pp)

swapExtensionForwards :: PP -> FilePath -> Maybe FilePath
swapExtensionForwards pp =
    swapExtension
        (ppConsumes . ppSpec $ pp)
        (ppProduces . ppSpec $ pp)

swapExtension :: Text -> Text -> FilePath -> Maybe FilePath
swapExtension fromExt toExt path = do
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
