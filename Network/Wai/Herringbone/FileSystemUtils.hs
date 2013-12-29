module Network.Wai.Herringbone.FileSystemUtils where

import System.IO hiding (FilePath)
import qualified Filesystem.Path.CurrentOS as F
import Filesystem.Path.CurrentOS (FilePath)
import Prelude hiding (FilePath)

import Network.Wai.Herringbone.Types

-- TODO: remove?
class ToFilePath a where
    toFilePath :: a -> F.FilePath

instance ToFilePath LogicalPath where
    toFilePath = F.concat . map F.fromText . fromLogicalPath

makeTempFile :: FilePath -> FilePath -> IO F.FilePath
makeTempFile sourceName workingDir = do
    let template = F.encodeString sourceName
    (path, handle) <- openTempFile template (F.encodeString workingDir)
    hClose handle
    return (F.decodeString path)
