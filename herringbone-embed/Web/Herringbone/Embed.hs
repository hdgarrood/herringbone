{-# LANGUAGE TemplateHaskell #-}
module Web.Herringbone.Embed where

import Control.Monad (forM, (>=>), when)
import Language.Haskell.TH.Syntax (Q, Exp(..), Lit(..), runIO)
import Data.FileEmbed (embedDir)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Filesystem.Path.CurrentOS as F
import qualified Data.Text as T

import Web.Herringbone

-- | Precompile and embed all assets into your source code. Call this function
-- in a Template Haskell splice. Any asset compilation failures will result in
-- a compile error.
--
-- For example:
--
-- > assets :: [(LogicalPath, ByteString)]
-- > assets = $(embedAssets
--      (herringbone
--          (setSourceDir "assets" . setDestDir ".compiled_assets"))
--      )
embedAssets :: IO Herringbone -> Q Exp
embedAssets iohb = do
    hb <- runIO iohb
    errs <- runIO (precompile hb)
    when (not . null $ errs) $
        fail ("the following assets failed to compile:\n" ++
            unlines (map show errs))

    SigE expr' _ <- embedDir (F.encodeString $ hbDestDir hb)
    let expr = transformFiles expr'

    type_ <- [t| [(LogicalPath, ByteString)] |]
    return $ SigE expr type_
    where
    transformFiles (ListE tups) =
        let f (TupE [LitE (StringL path), contents]) =
                TupE [logicalPathExp path, contents]
            f _ = error "unexpected Exp in precompileEmbed"
        in ListE $ map f tups
    transformFiles _ = error "unexpected Exp in precompileEmbed"
    logicalPathExp = logicalPathToExp . stringToLogicalPath
    stringToLogicalPath = unsafeMakeLogicalPath . T.splitOn "/" . T.pack
    logicalPathToExp logicalPath =
        AppE
            (VarE 'unsafeMakeLogicalPath)
            (ListE (map (LitE . StringL . T.unpack)
                        (fromLogicalPath logicalPath)))
