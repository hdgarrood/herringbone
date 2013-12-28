module Network.Wai.Herringbone.FindAsset where

import Network.Wai.Herringbone.LocateAssets
import Network.Wai.Herringbone.BuildAsset
import Network.Wai.Herringbone.Types
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath)

findAsset :: Herringbone
          -> LogicalPath
          -> IO (Either AssetError BundledAsset)
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
            -> IO (Either AssetError BundledAsset)
buildAsset' hb path srcPath pps = do
    result <- buildAsset hb path srcPath pps
    return $ mapLeft AssetCompileError result
    where
    mapLeft :: (a -> b) -> Either a r -> Either b r
    mapLeft f (Left x)  = Left $ f x
    mapLeft _ (Right x) = Right x
