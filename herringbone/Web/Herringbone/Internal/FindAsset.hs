module Web.Herringbone.Internal.FindAsset where

import Web.Herringbone.Internal.GetBuildMapping
import Web.Herringbone.Internal.BuildAsset
import Web.Herringbone.Internal.Types

-- | The most important function in this library. Attempts to find the asset
-- referenced by the given 'LogicalPath', compiles it if necessary (based on
-- file modification time), and returns it to you as an 'Asset' (or an
-- 'AssetError', if something went wrong).
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
