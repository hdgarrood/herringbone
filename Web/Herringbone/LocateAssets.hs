-- | This module deals with locating assets on the disk, and calculating how to
-- create assets which need preprocessing.
module Web.Herringbone.LocateAssets where

import Data.Maybe
import Data.Text (Text)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import Prelude hiding (FilePath)

import Web.Herringbone.Types

locateAssets :: Herringbone -> LogicalPath -> IO [(FilePath, [PP])]
locateAssets hb logPath = do
    let sourceDirs = hbSourceDirs hb
    let pps        = hbPPs hb
    let pathPieces = fromLogicalPath logPath
    assets <- sequence $ map (getAssetsFrom pps pathPieces) sourceDirs
    return $ concat assets

getAssetsFrom :: PPs
              -> [Text]      -- ^ requested path pieces
              -> FilePath    -- ^ Directory to look in
              -> IO [(FilePath, [PP])]
getAssetsFrom _   []          _    = return []
getAssetsFrom pps pathPieces' dir' = do
    let pathPieces        = map F.fromText pathPieces'
    let dir               = foldl (</>) dir' (init pathPieces)
    let assetName         = last pathPieces

    exists <- F.isDirectory dir
    if exists
        then do contents <- F.listDirectory dir
                let filenames = getAssetsFrom'
                                    pps
                                    assetName
                                    (map F.filename contents)
                return $ map (\(path, xs) -> (dir </> path, xs)) filenames
        else return []

-- Given a list of preprocessors, the path of an asset we want to serve, and
-- a list of potential source files, return a list of all the files which could
-- be used as a source for that file, together with the preprocessors which
-- would need to be applied (in the correct order) to preprocess that file.
--
-- For example, given preprocessors for "sass" and "erb", the asset path
-- "style.css", and the following list of potential files:
--
--  "style.css"
--  "style.css.sass"
--  "style.css.sass.erb"
--  "style.css.unrecognised-ext"
--  "javascript.js"
--
-- we should get back:
--
--  [ ("style.css", [])
--  , ("style.css.sass", [sass])
--  , ("style.css.sass.erb", [erb, sass])
--  ]
getAssetsFrom' :: PPs
               -> FilePath      -- ^ Asset to serve
               -> [FilePath]    -- ^ Potential source files
               -> [(FilePath, [PP])]
getAssetsFrom' pps assetPath = catMaybes . map resolve
    where
    resolve :: FilePath -> Maybe (FilePath, [PP])
    resolve fp = fmap (\xs -> (fp, xs)) $ (resolvePPs pps assetPath fp)

-- Can we apply a sequence of the given preprocessors to the given source file
-- path to get the given asset? If so, return the list of preprocessors which
-- should be applied to it to make this happen.
resolvePPs :: PPs -> FilePath -> FilePath -> Maybe [PP]
resolvePPs pps assetPath source = do
    exts   <- getExtraExtensions assetPath source
    ppList <- sequence $ map (\e -> lookupPP e pps) exts
    return ppList

-- Check if a file path is formed from another file path plus a list of
-- extensions, and if so, return those extensions, in reverse order.
-- Eg:
--  getExtraExtensions "game.js" "game.js.coffee"     == Just ["coffee"]
--  getExtraExtensions "style.css" "game.js.coffee"   == Nothing
--  getExtraExtensions "game.js" "game.js.coffee.erb" == Just ["erb", "coffee"]
getExtraExtensions :: FilePath -> FilePath -> Maybe [Text]
getExtraExtensions fp fpWithExts = do
    stripped <- F.stripPrefix fp fpWithExts
    return $ (reverse . F.extensions) stripped
