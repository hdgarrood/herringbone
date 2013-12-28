module Network.Wai.Herringbone.Types where

import Data.Char
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import qualified Filesystem.Path.CurrentOS as F
import Filesystem.Path.CurrentOS (FilePath)
import Prelude hiding (FilePath)

class ToLazyByteString a where
    toLazyByteString :: a -> BL.ByteString

instance ToLazyByteString String where
    toLazyByteString = BL.pack . map (fromIntegral . ord)

instance ToLazyByteString FilePath where
    toLazyByteString = BL.fromChunks . (: []) . F.encode

type CompileError = String

-- | PP
data PP = PP
    { ppExtension :: Text
    -- ^ The file extension this preprocessor acts upon, eg "sass" or "coffee"
    , ppAction    :: FilePath -> FilePath -> IO (Maybe CompileError)
    -- ^ an function which takes a source path and a destination path and
    -- returns an action which performs the compilation
    }

-- | Yes, there's a bit of redundancy here...
newtype PPs = PPs { unPPs :: M.Map Text PP }

noPPs :: PPs
noPPs = PPs M.empty

supportedBy :: PPs -> Text -> Bool
supportedBy pps = flip M.member (unPPs pps)

supportedExts :: PPs -> [Text]
supportedExts = M.keys . unPPs

insertPP :: PP -> PPs -> PPs
insertPP pp = PPs . M.insert (ppExtension pp) pp . unPPs

lookupPP :: Text -> PPs -> Maybe PP
lookupPP ext = M.lookup ext . unPPs

fromList :: [PP] -> PPs
fromList = foldr insertPP noPPs

data Herringbone = Herringbone
    { hbSourceDirs :: [FilePath]
    , hbDestDir    :: FilePath
    , hbPPs        :: PPs
    }
