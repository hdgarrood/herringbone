module Web.Herringbone.FindAsset where

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath)

import Web.Herringbone.LocateAssets
import Web.Herringbone.BuildAsset
import Web.Herringbone.Types

findAsset :: Herringbone
          -> LogicalPath
          -> IO (Either AssetError Asset)
findAsset hb path = do
    assets <- locateAssets hb path
    case assets of
        []               -> return . Left $ AssetNotFound
        [(srcPath, pps)] -> buildAsset' hb path srcPath pps
        xs               -> return . Left $ AmbiguousSources (map fst xs)

buildAsset' :: Herringbone
            -> LogicalPath
            -> FilePath
            -> [PP]
            -> IO (Either AssetError Asset)
buildAsset' hb path srcPath pps = do
    result <- buildAsset hb path srcPath pps
    return $ mapLeft AssetCompileError result
    where
    mapLeft :: (a -> b) -> Either a r -> Either b r
    mapLeft f (Left x)  = Left $ f x
    mapLeft _ (Right x) = Right x
