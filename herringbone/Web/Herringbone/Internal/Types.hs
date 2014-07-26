module Web.Herringbone.Internal.Types where

import Control.Monad.Reader
import Control.Applicative
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import System.IO hiding (FilePath)
import qualified Data.ByteString as B
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import Filesystem.Path.CurrentOS (FilePath)
import Prelude hiding (FilePath)

-- | A value describing an error that occurred while trying to produce an
-- 'Asset'.
data AssetError = AssetNotFound
                | AssetCompileError CompileError
                | AmbiguousSources [FilePath]
                deriving (Show, Eq)

-- | Data which is given to preprocessors on the off-chance that they need it
-- (eg, Fay)
data PPReader = PPReader
    { ppReaderHb          :: Herringbone
    -- ^ The Herringbone which was used to build the asset
    , ppReaderSourcePath  :: FilePath
    -- ^ The file path to the source file
    , ppReaderPPs         :: [PP]
    -- ^ Preprocessors being invoked. Currently this will only ever have zero
    -- or one elements; in the future, Herringbone may be able to run multiple
    -- preprocessors on a single file.
    }

-- | A monad in which preprocessor actions happen.
newtype PPM a = PPM { unPPM :: ReaderT PPReader IO a }
    deriving (Functor, Applicative, Monad, MonadIO, (MonadReader PPReader))

runPPM :: PPM a -> PPReader -> IO a
runPPM comp = runReaderT (unPPM comp)

-- | A preprocessor something which is run on the asset before it is served.
-- Preprocessors are run when a file matches its rule.  For example, if you
-- have a preprocessor which takes \"coffee\" files and emits \"js\" files,
-- there is a file named \"application.coffee\", and you request
-- \"application.js\", Herringbone will run the coffee preprocessor on
-- \"application.coffee\" and serve you the result.
data PP = PP
    { ppName     :: Text
    -- ^ Identifies a preprocessor. Mainly useful for debugging compile errors.
    , ppConsumes :: Text
    -- ^ Extension for files this preprocessor consumes.
    , ppProduces :: Text
    -- ^ Extension for files this preprocessor produces.
    , ppAction :: PPAction
    -- ^ Performs the compilation.
    }

instance Show PP where
    show (PP name consumes produces _) = concat
        [ "<PP ", T.unpack name,
            " (", T.unpack consumes, " -> ", T.unpack produces, ")>"
        ]

-- | A function which performs the compilation.
type PPAction =
    B.ByteString
    -- ^ Input file contents
    -> PPM (Either CompileError B.ByteString)
    -- ^ Output file contents, or a compile error.

-- | A string which should contain information about why an asset failed to
-- compile.
type CompileError = B.ByteString

-- | A collection of preprocessors. This can store many preprocessors which
-- produce files with the same extension, but may not store more than one
-- preprocessor which consumes files of a particular extension.
newtype PPs = PPs { unPPs :: Map Text PP }
    deriving (Show)

emptyPPs :: PPs
emptyPPs = PPs M.empty

-- | Given a file extension, find the preprocessor (if any) which consumes it.
lookupPP :: Text -> PPs -> Maybe PP
lookupPP ext pps = M.lookup ext (unPPs pps)

-- | Inserts a preprocessor into a PPs. If a preprocessor already exists with
-- the given extension, it is discarded.
insertPP :: PP -> PPs -> PPs
insertPP pp pps = PPs $ M.insert (ppConsumes pp) pp (unPPs pps)

-- | Turn a list of PPs into a proper 'PPs'.
fromList :: [PP] -> PPs
fromList = foldr insertPP emptyPPs

-- | A BuildSpec specifies how an asset should be built.
data BuildSpec = BuildSpec
    { bsSourcePath :: FilePath  -- ^ Source path (relative)
    , bsDestPath   :: FilePath  -- ^ Destination path (again, relative)
    , bsPP         :: (Maybe PP)  -- ^ Preprocessor to run (if any)
    }
    deriving (Show)

-- | A BuildMapping contains the information to build all of the assets
-- Herringbone is aware of.
type BuildMapping = [BuildSpec]

-- | The \'main\' datatype in this library.  All of the important functions
-- will take a 'Herringbone' as their first argument.
data Herringbone = Herringbone
    { herringboneSettings :: HerringboneSettings
    , herringboneStartTime :: UTCTime
    }

-- | Contains configuration.
data HerringboneSettings = HerringboneSettings
    { settingsSourceDir :: FilePath
    -- ^ The directory to take asset sources from.
    , settingsDestDir    :: FilePath
    -- ^ Where to copy assets to after they've been compiled.
    , settingsPPs        :: PPs
    -- ^ Preprocessors
    , settingsVerbose    :: Bool
    -- ^ Dump debugging data to stdout on every request.
    }
    deriving (Show)

type ConfigBuilder = HerringboneSettings -> HerringboneSettings

-- | The directory where Herringbone will look when searching for assets.
hbSourceDir :: Herringbone -> FilePath
hbSourceDir = settingsSourceDir . herringboneSettings

-- | The directory to place assets in after compilation.
hbDestDir :: Herringbone -> FilePath
hbDestDir = settingsDestDir . herringboneSettings

-- | The collection of preprocessors that will be used when preprocessing
-- assets.
hbPPs :: Herringbone -> PPs
hbPPs = settingsPPs . herringboneSettings

-- | True iff the 'Herringbone' has the verbose setting enabled.
hbVerbose :: Herringbone -> Bool
hbVerbose = settingsVerbose . herringboneSettings

-- | Log a message to stdout if hbVerbose is enabled.
verbosePut :: Herringbone -> String -> IO ()
verbosePut hb msg = when (hbVerbose hb) $ do
    putStrLn ("herringbone: " ++ msg)
    hFlush stdout

-- | All assets in Herringbone are referenced by their logical path. This is
-- the path to an asset, relative to the source directory.
newtype LogicalPath = LogicalPath { fromLogicalPath :: [Text] }
    deriving (Show, Eq)

-- | Create a LogicalPath from a list of path segments. For example,
-- @[\"data\", \"dogs.txt\"]@ would map to data/dogs.txt (relative to the
-- source directory).  This returns Nothing if the path would be unsafe (that
-- is, if it contains \"..\"), to prevent directory traversal attacks.
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
    , assetModifiedTime :: UTCTime
    -- ^ Modification time of the asset's source file.
    }

assetContent :: Asset -> IO B.ByteString
assetContent = F.readFile . assetFilePath

instance Show Asset where
    show (Asset size sourcePath filePath modifiedTime) =
        "Asset { " ++
        "assetSize = " ++ show size ++ ", " ++
        "assetSourcePath = " ++ show sourcePath ++ ", " ++
        "assetFilePath = " ++ show filePath ++ ", " ++
        "assetModifiedTime = " ++ showTime modifiedTime ++ " }"

        where
        showTime = formatTime defaultTimeLocale (dateTimeFmt defaultTimeLocale)
