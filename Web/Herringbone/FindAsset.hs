module Web.Herringbone.FindAsset where

import Web.Herringbone.LocateAssets
import Web.Herringbone.BuildAsset
import Web.Herringbone.Types

findAsset :: Herringbone
          -> LogicalPath
          -> IO (Either AssetError Asset)
findAsset hb path = do
    mapping <- getBuildMapping hb
    findAssetWithMapping hb path mapping

findAssetWithMapping :: Herringbone
                     -> LogicalPath
                     -> BuildMapping
                     -> IO (Either AssetError Asset)
findAssetWithMapping hb path mapping =
    case specs of
            []  -> return . Left $ AssetNotFound
            [x] -> buildAsset' hb x
            xs  -> return . Left $ AmbiguousSources (map getSource xs)
    where
    getSource (BuildSpec s _ _) = s
    getDest (BuildSpec _ d _) = d
    destPath = toFilePath path
    specs = filter ((== destPath) . getDest) mapping

buildAsset' :: Herringbone
            -> BuildSpec
            -> IO (Either AssetError Asset)
buildAsset' hb spec = do
    result <- buildAsset hb spec
    return $ mapLeft AssetCompileError result
    where
    mapLeft :: (a -> b) -> Either a r -> Either b r
    mapLeft f (Left x)  = Left $ f x
    mapLeft _ (Right x) = Right x
