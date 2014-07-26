module Web.Herringbone.Internal.Precompile where

import Control.Monad (forM)
import qualified Filesystem.Path.CurrentOS as F
import qualified Data.Text as T

import Web.Herringbone.Internal.Types
import Web.Herringbone.Internal.FindAsset
import Web.Herringbone.Internal.GetBuildMapping (getBuildMapping)

-- | Precompiles all assets, returning a list of the logical paths of assets
-- that failed to compile (if any) together with 'AssetError' values describing
-- what went wrong.
precompile :: Herringbone -> IO [(LogicalPath, AssetError)]
precompile hb = do
    mapping <- getBuildMapping hb
    results <- forM mapping $ \(BuildSpec _ destPath _) -> do
        let path = toLogicalPath destPath
        either invalidPath compile path
    return $ concat results
    where
    toLogicalPath filePath =
        case F.toText filePath of
            Left approximation ->
                Left (unsafeMakeLogicalPath [approximation])
            Right (T.splitOn "/" -> pieces) ->
                Right (unsafeMakeLogicalPath pieces)

    invalidPath path = return [(path, AssetNotFound)]

    compile path = do
        asset <- findAsset hb path
        return $ case asset of
            Right _  -> []
            Left err -> [(path, err)]
