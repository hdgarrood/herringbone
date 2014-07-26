module Web.Herringbone.Preprocessor.Fay (makeFayPP) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Applicative
import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.IO
import System.IO.Temp
import System.FilePath
import qualified Filesystem.Path.CurrentOS as F
import qualified Fay
import qualified Fay.Config as Fay
import Web.Herringbone

-- | Make a Fay preprocessor which will compile Fay files (which end \".hs\")
-- to JavaScript (\".js\"), using the supplied 'Fay.Config'.
makeFayPP :: Fay.Config -> PP
makeFayPP config = PP
    { ppName     = "fay"
    , ppConsumes = "hs"
    , ppProduces = "js"
    , ppAction   = compile config
    }

compile :: Fay.Config
        -> ByteString
        -> PPM (Either CompileError ByteString)
compile config _ = do
        pps <- asks ppReaderPPs
        if ppName (head pps) /= "fay"
            then return $ Left
                "Fay must appear first in the preprocessor chain"
            else compileUnsafe config

compileUnsafe :: Fay.Config -> PPM (Either CompileError ByteString)
compileUnsafe config' = do
    sourceDir <- (es . hbSourceDir) <$> asks ppReaderHb
    let config = Fay.addConfigDirectoryIncludePaths [sourceDir] config'
    sourcePath <- fmap es $ asks ppReaderSourcePath

    liftIO $ do
        putStrLn "about to compile fay"
        putStrLn $ "config: " ++ show config
        putStrLn $ "sourcePath: " ++ show sourcePath
    result <- liftIO $ Fay.compileFile config sourcePath
    either (return . Left . toBS . wrapWithPre . Fay.showCompileError)
           (return . Right . toBS)
           result
    where
        es = F.encodeString
        toBS = B.pack . map (fromIntegral . ord)
        wrapWithPre str = "<pre>" ++ str ++ "</pre>"
