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

getBuildSpecBackwards :: Herringbone
                      -> LogicalPath
                      -> IO [BuildSpec]
getBuildSpecBackwards hb path = do
    undefined

getBuildSpecForwards :: Herringbone
                     -> FilePath
                     -> IO [BuildSpec]
getBuildSpecForwards hb sourcePath = return [spec]
    where
    (destPath, pp) =
        fromMaybe (sourcePath, Nothing) $ do
            extension <- F.extension path
            pp <- lookupConsumer (hbPPs hb)
            destPath <- swapExtensionFor pp sourcePath
            return (destPath, Just pp)
    spec = BuildSpec sourcePath destPath pp

serveWithPP :: Herringbone
            -> FilePath
            -> Maybe PP
            -> IO [BuildSpec]
serveWithPP hb path pp' = do
    fullSourcePath' <- searchForFile (hbSourceDirs hb) path
    maybe (return [])
          (\fullSourcePath -> do
                fullDestPath <- F.canonicalizePath $ hbDestDir hb </> destPath
                let spec = BuildSpec fullSourcePath fullDestPath pp'
                return [spec])
          fullSourcePath'

    where
    destPath = case pp' of
        Just pp -> fromJust $ swapExtensionFor pp path
        Nothing -> path

swapExtension :: Text -> Text -> FilePath -> Maybe FilePath
swapExtension fromExt toExt path = do
    guard $ F.hasExtension path fromExt
    return $ F.replaceExtension path toExt

swapExtensionFor :: PP -> FilePath -> Maybe FilePath
swapExtensionFor pp =
    swapExtension
        (ppConsumes . ppSpec $ pp)
        (ppProduces . ppSpec $ pp)

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
