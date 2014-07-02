module Web.Herringbone.Precompile where

import qualified Filesystem.Path.CurrentOS as F
import qualified Data.Text as T
import Control.Monad (forM, (>=>))

import Web.Herringbone.LocateAssets
import Web.Herringbone.FindAsset
import Web.Herringbone.Types

-- | Precompiles all assets.
precompile :: Herringbone -> IO [(LogicalPath, AssetError)]
precompile hb = do
    mapping <- getBuildMapping hb
    errs <- forM mapping $ \(BuildSpec _ destPath _) -> do
        let Just path = toLogicalPath $ destPath
        asset <- findAsset hb path
        case asset of
            Right _ -> return []
            Left err -> return [(path, err)]
    return $ concat errs

    where
    toLogicalPath =
        toMaybe F.toText >=>
        return . T.splitOn "/" >=>
        makeLogicalPath 
    toMaybe f x = case f x of
        Right y -> Just y
        Left _  -> Nothing
