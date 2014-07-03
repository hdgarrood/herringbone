module Web.Herringbone.Preprocessor.Fay (makeFayPP) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.IO
import System.IO.Temp
import System.FilePath
import qualified Filesystem.Path.CurrentOS as F
import qualified Fay
import qualified Fay.Compiler.Config as Fay
import Web.Herringbone

makeFayPP :: Fay.CompileConfig -> PP
makeFayPP config = PP
    { ppExtension = "hs"
    , ppAction    = compile config
    }

compile :: Fay.CompileConfig
        -> ByteString
        -> PPM (Either CompileError ByteString)
compile config _ = do
        pps <- asks ppReaderPPs
        if ppExtension (head pps) /= "hs"
            then return $ Left
                "Fay must appear first in the preprocessor chain"
            else compileUnsafe config

compileUnsafe :: Fay.CompileConfig -> PPM (Either CompileError ByteString)
compileUnsafe config' = do
    sourceDirs <- fmap (map es . hbSourceDirs) $ asks ppReaderHb
    let config = Fay.addConfigDirectoryIncludePaths sourceDirs config'
    sourcePath <- fmap es $ asks ppReaderSourcePath

    liftIO $ do
        putStrLn "about to compile fay"
        putStrLn $ "config: " ++ show config
        putStrLn $ "sourcePath: " ++ show sourcePath
    result <- liftIO $ Fay.compileFile config sourcePath
    either (return . Left . toBS . wrapWithPre . Fay.showCompileError)
           (return . Right . toBS . fst)
           result
    where
        es = F.encodeString
        toBS = B.pack . map (fromIntegral . ord)
        wrapWithPre str = "<pre>" ++ str ++ "</pre>"
