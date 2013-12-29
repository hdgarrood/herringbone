module Network.Wai.Herringbone.Types where

import Data.Char
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T
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

data AssetError = AssetNotFound
                | AssetCompileError CompileError
                | AmbiguousSources [FilePath]

type CompileError = String

-- | PP
data PP = PP
    { ppExtension :: Text
    -- ^ The file extension this preprocessor acts upon, eg "sass" or "coffee"
    , ppAction    :: FilePath -> FilePath -> IO (Maybe CompileError)
    -- ^ an function which takes a source path and a destination path and
    -- returns an action which performs the compilation
    }

instance Show PP where
    show pp = "PP { ppExtension = " ++ T.unpack (ppExtension pp) ++
                 ", ppAction = <not showable> }"

-- | Yes, there's a bit of redundancy here...
newtype PPs = PPs { unPPs :: M.Map Text PP }
    deriving (Show)

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
fromList ppList = insertAllPPs ppList noPPs

insertAllPPs :: [PP] -> PPs -> PPs
insertAllPPs ppList pps = foldr insertPP pps ppList

data Herringbone = Herringbone
    { hbSourceDirs :: [FilePath]
    , hbDestDir    :: FilePath
    , hbPPs        :: PPs
    }
    deriving (Show)

newtype LogicalPath = LogicalPath { fromLogicalPath :: [Text] }

data BundledAsset = BundledAsset
    { assetSize         :: Integer
    -- ^ Size of the asset in bytes
    , assetSourcePath   :: FilePath
    -- ^ Path to the asset's source file on disk
    , assetFilePath     :: FilePath
    -- ^ Path to the preprocessed asset on disk
    , assetLogicalPath  :: LogicalPath
    -- ^ Logical path supplied to Herringbone
    , assetModifiedTime :: UTCTime
    -- ^ Modification time of the asset's source file
    }
