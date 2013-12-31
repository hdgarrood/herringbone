module Network.Wai.Herringbone.Types where

import Data.Char
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Filesystem.Path.CurrentOS as F
import Filesystem.Path.CurrentOS (FilePath)
import Prelude hiding (FilePath)

class ToLazyByteString a where
    toLazyByteString :: a -> BL.ByteString

instance ToLazyByteString String where
    toLazyByteString = BL.pack . map (fromIntegral . ord)

instance ToLazyByteString FilePath where
    toLazyByteString = toLazyByteString . F.encode

instance ToLazyByteString B.ByteString where
    toLazyByteString = BL.fromChunks . (: [])

data AssetError = AssetNotFound
                | AssetCompileError CompileError
                | AmbiguousSources [FilePath]
                deriving (Show, Eq)

-- | A string which should contain information about why an asset failed to
-- compile.
type CompileError = B.ByteString

-- | A preprocessor something which is run on the asset before it is served.
-- Preprocessors are run when a file extension matches the preprocessor
-- extension. For example, if you have a preprocessor for \"coffee\" files, you
-- request \"application.js\", and there is a file named
-- \"application.js.coffee\", Herringbone will run the coffee preprocessor on
-- that file and serve you the result.
--
-- You can add more preprocessors by adding more file extensions;
-- \"application.js.coffee.erb\" will be preprocessed first by \"erb\", then by
-- \"coffee\" (assuming you have registered preprocessors for those files).
data PP = PP
    { ppExtension :: Text
    -- ^ The file extension this preprocessor acts upon, eg \"sass\" or
    -- \"hamlet\"
    , ppAction    :: B.ByteString -> IO (Either CompileError B.ByteString)
    -- ^ Perform the preprocessing.
    }

instance Show PP where
    show pp = "<PP: " ++ show (ppExtension pp) ++ ">"

-- | Beware: This instance is only here for testing. It only looks at the
-- extensions to decide whether two 'PP's are equal. Don't use this!
instance Eq PP where
    (PP ext1 _) == (PP ext2 _) = ext1 == ext2

instance Ord PP where
    compare (PP ext1 _) (PP ext2 _) = compare ext1 ext2

-- | A collection of preprocessors.
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

-- | The \'main\' datatype in this library. Just a container for the
-- configuration. All of the important functions will take a 'Herringbone' as
-- their first argument.
data Herringbone = Herringbone
    { hbSourceDirs :: [FilePath]
    -- ^ A list of source directories; this is where assets should be placed.
    , hbDestDir    :: FilePath
    -- ^ Where to copy assets to after they've been compiled.
    , hbPPs        :: PPs
    -- ^ Preprocessors
    }
    deriving (Show)

-- | All assets in Herringbone are referenced by their logical path. This is
-- the path to an asset, relative to any of the source directories.
newtype LogicalPath = LogicalPath { fromLogicalPath :: [Text] }
    deriving (Show)

-- | Create a LogicalPath from a list of Text values. This returns Nothing if
-- the path would be unsafe (that is, if it contains \"..\"), to prevent
-- directory traversal attacks.
makeLogicalPath :: [Text] -> Maybe LogicalPath
makeLogicalPath xs = if safe xs then Just $ LogicalPath xs else Nothing
    where
        safe = all (not . (==) "..")

-- | Create a LogicalPath without checking any of the values.
unsafeMakeLogicalPath :: [Text] -> LogicalPath
unsafeMakeLogicalPath = LogicalPath

toFilePath :: LogicalPath -> FilePath
toFilePath = F.concat . map F.fromText . fromLogicalPath

-- | A preprocessed asset. Any function that returns this will already have
-- done the preprocessing (if necessary).
data Asset = Asset
    { assetSize         :: Integer
    -- ^ Size of the asset in bytes.
    , assetSourcePath   :: FilePath
    -- ^ Path to the asset's source file on disk.
    , assetFilePath     :: FilePath
    -- ^ Path to the preprocessed asset on disk. Note that assets which do not
    -- require preprocessing will still be copied to the destination directory.
    , assetLogicalPath  :: LogicalPath
    -- ^ The logical path referencing this asset.
    , assetModifiedTime :: UTCTime
    -- ^ Modification time of the asset's source file.
    }

instance Show Asset where
    show (Asset size sourcePath filePath logicalPath modifiedTime) =
        "BundledAsset { " ++
        "assetSize = " ++ show size ++ ", " ++
        "assetSourcePath = " ++ show sourcePath ++ ", " ++
        "assetFilePath = " ++ show filePath ++ ", " ++
        "assetLogicalPath = " ++ show logicalPath ++ ", " ++
        "assetModifiedTime = " ++ showTime modifiedTime ++ " }"

        where
        showTime = formatTime defaultTimeLocale (dateTimeFmt defaultTimeLocale)
