module Network.Wai.Herringbone.FileSystemUtils where

import System.IO.Temp
import WaiAppStatic.Types
import qualified Filesystem.Path.CurrentOS as F
import Filesystem (getAppCacheDirectory)
import Prelude hiding (FilePath, concat)

class ToFilePath a where
    toFilePath :: a -> F.FilePath

instance ToFilePath Pieces where
    toFilePath = F.concat . map F.fromText . map fromPiece

instance ToFilePath Piece where
    toFilePath = F.fromText . fromPiece

openTempFile' :: IO F.FilePath
openTempFile' = do
    let template = "herringbone-intermediate."
    dir <- getAppCacheDirectory "haskell-herringbone"
    fmap (F.decodeString . fst) $ openTempFile template (F.encodeString dir)
