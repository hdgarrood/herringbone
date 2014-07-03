{-# LANGUAGE TemplateHaskell #-}
module Web.Herringbone.Precompile where

import Language.Haskell.TH.Syntax (Q, Exp(..), Lit(..), runIO)
import Data.FileEmbed (embedDir)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Filesystem.Path.CurrentOS as F
import qualified Data.Text as T
import Control.Monad (forM, (>=>))

import Web.Herringbone.GetBuildMapping
import Web.Herringbone.FindAsset
import Web.Herringbone.Types

-- | Precompiles all assets.
precompile :: Herringbone -> IO [(LogicalPath, AssetError)]
precompile hb = do
    mapping <- getBuildMapping hb
    results <- forM mapping $ \(BuildSpec _ destPath _) -> do
        let Just path = toLogicalPath $ destPath
        asset <- findAsset hb path
        case asset of
            Right _ -> return []
            Left err -> return [(path, err)]
    return $ concat results

    where
    toLogicalPath =
        toMaybe F.toText >=>
        return . T.splitOn "/" >=>
        makeLogicalPath 
    toMaybe f x = case f x of
        Right y -> Just y
        Left _  -> Nothing

-- | Precompile and embed all assets into your source code. Call this function
-- in a Template Haskell splice.
--
-- Returns: (Errors, Files) where:
--
-- > type Errors = [(LogicalPath, AssetError)]
-- > type Files = [(LogicalPath, ByteString)] 
--
-- The second component is a mapping of filenames to file contents.
embedAssets :: IO Herringbone -> Q Exp
embedAssets iohb = do
    hb <- runIO iohb
    errs <- runIO (precompile hb)
    let errsExp = ListE $ map (\(path, err) ->
                        TupE [logicalPathToExp path, errToExp err]) errs
    SigE filesExp' _ <- embedDir (F.encodeString $ hbDestDir hb)
    let filesExp = transformFiles filesExp'
    let expr = TupE [errsExp, filesExp]

    type_ <- [t| ([(LogicalPath, AssetError)], [(LogicalPath, ByteString)]) |]
    return $ SigE expr type_
    where
    logicalPathToExp logicalPath =
        AppE
            (ConE 'LogicalPath)
            (ListE (map (LitE . StringL . T.unpack)
                        (fromLogicalPath logicalPath)))
    errToExp e = case e of
        AssetNotFound ->
            ConE 'AssetNotFound
        AssetCompileError err ->
            AppE (ConE 'AssetCompileError) $ compileErrToExp err
        AmbiguousSources srcs ->
            AppE (ConE 'AmbiguousSources) $ sourcesToExp srcs
    compileErrToExp err = LitE (StringL (B8.unpack err))
    sourcesToExp srcs = ListE $ map filePathToExp srcs
    -- Just use a literal because we have an IsString instance
    filePathToExp path = LitE (StringL (F.encodeString path))

    transformFiles (ListE tups) = 
        let f (TupE [LitE (StringL path), contents]) =
                TupE [logicalPathExp path, contents]
            f _ = error "unexpected Exp in precompileEmbed"
        in ListE $ map f tups
    transformFiles _ = error "unexpected Exp in precompileEmbed"

    logicalPathExp = logicalPathToExp . stringToLogicalPath
    stringToLogicalPath = unsafeMakeLogicalPath . T.splitOn "/" . T.pack
