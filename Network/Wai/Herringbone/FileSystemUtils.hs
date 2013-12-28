module Network.Wai.Herringbone.FileSystemUtils where

import System.IO
import qualified Filesystem.Path.CurrentOS as F
import Filesystem (getAppCacheDirectory)
import Prelude hiding (FilePath, concat)

import Network.Wai.Herringbone.Types

class ToFilePath a where
    toFilePath :: a -> F.FilePath

instance ToFilePath LogicalPath where
    toFilePath = F.concat . map F.fromText . fromLogicalPath

makeTempFile :: IO F.FilePath
makeTempFile = do
    let template = "herringbone-intermediate."
    dir <- getAppCacheDirectory "haskell-herringbone"
    (path, handle) <- openTempFile template (F.encodeString dir)
    hClose handle
    return (F.decodeString path)
