module Web.Herringbone.Types where

import Control.Monad.Reader
import Control.Applicative
import Data.Char
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import System.IO hiding (FilePath)
import Data.Text (Text)
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

-- | Data which is given to preprocessors on the off-chance that they need it
-- (eg, Fay)
data PPReader = PPReader
    { ppReaderHb          :: Herringbone 
    -- ^ The Herringbone which was used to build the asset
    , ppReaderLogicalPath :: LogicalPath
    -- ^ The Logical path of the requested asset.
    , ppReaderSourcePath  :: FilePath
    -- ^ The file path to the source file
    , ppReaderPPs         :: [PP]
    -- ^ Preprocessors being invoked.
    }

ppReaderFileName :: PPReader -> FilePath
ppReaderFileName = F.fromText . last . fromLogicalPath . ppReaderLogicalPath

-- | A monad in which preprocessor actions happen.
newtype PPM a = PPM { unPPM :: ReaderT PPReader IO a }
    deriving (Functor, Applicative, Monad, MonadIO, (MonadReader PPReader))

runPPM :: PPM a -> PPReader -> IO a
runPPM comp readerData = runReaderT (unPPM comp) readerData

-- | A preprocessor something which is run on the asset before it is served.
-- Preprocessors are run when a file matches its rule.  For example, if you
-- have a preprocessor which takes \"coffee\" files and emits \"js\" files,
-- there is a file named \"application.coffee\", and you request
-- \"application.js\", Herringbone will run the coffee preprocessor on that
-- \"application.coffee\" and serve you the result.
data PP = PP
    { ppSpec   :: PPSpec
    , ppAction :: PPAction
    }

instance Show PP where
    show (PP spec _) = "PP { ppSpec = " ++ show spec ++ ", ppAction = <???> }"

-- | A function which performs the compilation.
type PPAction = B.ByteString -> PPM (Either CompileError B.ByteString)

-- | A string which should contain information about why an asset failed to
-- compile.
type CompileError = B.ByteString

-- | Information describing a preprocessor.
data PPSpec = PPSpec
    { ppName     :: Text
    -- ^ Identifies a preprocessor. Mainly useful for debugging compile errors.
    , ppConsumes :: Text
    -- ^ Extension for files this preprocessor consumes.
    , ppProduces :: Text
    -- ^ Extension for files this preprocessor produces.
    } deriving (Show, Eq, Ord)

-- | A collection of preprocessors. This can store many preprocessors which
-- produce files with the same extension, but may not store more than one
-- preprocessor which consumes files of a particular extension.
newtype PPs = PPs { unPPs :: [PP] }
    deriving (Show)

noPPs :: PPs
noPPs = PPs []

-- | Given a file extension, find the preprocessor (if any) which consumes it.
lookupConsumer :: PPs -> Text -> Maybe PP
lookupConsumer pps ext = case filter (consumes ext) (unPPs pps) of
    []  -> Nothing
    [x] -> Just x
    xs  -> error $
        "herringbone: lookupConsumer got " ++ show xs ++ ". This is a bug. :(\
        \ Please report it: <https://github.com/hdgarrood/herringbone>"
    where
    consumes e = (==) e . ppConsumes . ppSpec

-- | Given a file extension, find the preprocessors which can produce it.
lookupProducers :: PPs -> Text -> [PP]
lookupProducers pps ext = filter (produces ext) $ unPPs pps
    where
    produces e = (==) e . ppProduces . ppSpec

-- | Inserts a preprocessor into a PPs safely.
insertPP :: PP -> PPs -> Maybe PPs
insertPP pp pps = do
    let newExt = ppConsumes $ ppSpec pp
    guard (not . hasConsumerOf newExt $ pps)
    return $ unsafeInsertPP pp pps
    where
    hasConsumerOf e = isJust . flip lookupConsumer e

-- | Inserts a preprocessor into a PPs unsafely.
unsafeInsertPP :: PP -> PPs -> PPs
unsafeInsertPP pp (PPs pps) = PPs (pps ++ [pp])

-- | Turn a list of PPs into a proper 'PPs'.
fromList :: [PP] -> Maybe PPs
fromList ppList = insertAllPPs ppList noPPs

insertAllPPs :: [PP] -> PPs -> Maybe PPs
insertAllPPs ppList pps = allInserts pps
    where
    inserts    = map insertPP ppList
    allInserts = foldl (>=>) (Just) inserts

-- | Turn a list of PPs into a proper 'PPs', raising an error if they cannot be
-- converted.
fromList' :: [PP] -> PPs
fromList' = fromJust . fromList

-- | A BuildSpec specifies how an asset should be built.
data BuildSpec = BuildSpec
                    FilePath    -- ^ Source path (relative)
                    FilePath    -- ^ Destination path (again, relative)
                    (Maybe PP)  -- ^ Preprocessor to run (if any)

-- | The \'main\' datatype in this library. Contains all configuration.  All of
-- the important functions will take a 'Herringbone' as their first argument.
data Herringbone = Herringbone
    { hbSourceDirs :: [FilePath]
    -- ^ A list of source directories; this is where assets should be placed.
    , hbDestDir    :: FilePath
    -- ^ Where to copy assets to after they've been compiled.
    , hbPPs        :: PPs
    -- ^ Preprocessors
    , hbVerbose    :: Bool
    -- ^ Dump debugging data to stdout on every request.
    }
    deriving (Show)

-- | Log a message to stdout if hbVerbose is enabled.
verbosePut :: Herringbone -> String -> IO ()
verbosePut hb msg = when (hbVerbose hb) $ do
    putStrLn ("herringbone: " ++ msg)
    hFlush stdout

-- | All assets in Herringbone are referenced by their logical path. This is
-- the path to an asset, relative to any of the source directories.
newtype LogicalPath = LogicalPath { fromLogicalPath :: [Text] }
    deriving (Show, Eq)

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
