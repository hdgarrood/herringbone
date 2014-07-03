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
            [x] -> buildAsset hb x
            xs  -> return . Left $ AmbiguousSources (map getSource xs)
    where
    getSource (BuildSpec s _ _) = s
    getDest (BuildSpec _ d _) = d
    destPath = toFilePath path
    specs = filter ((== destPath) . getDest) mapping
