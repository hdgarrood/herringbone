module Network.Wai.Herringbone where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Time
import Data.Time.Clock.POSIX
import System.Posix.Types (EpochTime)
import Data.Char
import Data.Monoid
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai
import Network.Wai.Application.Static
import WaiAppStatic.Types
import Network.HTTP.Types
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import System.PosixCompat.Files

import Network.Wai.Herringbone.FileSystemUtils

class ToLazyByteString a where
    toLazyByteString :: a -> BL.ByteString

instance ToLazyByteString String where
    toLazyByteString = BL.pack . map (fromIntegral . ord)

instance ToLazyByteString FilePath where
    toLazyByteString = BL.fromChunks . (: []) . F.encode

type CompileError = String

-- | PP
data PP = PP
    { ppExtension :: String
    -- ^ The file extension this preprocessor acts upon, eg "sass" or "coffee"
    , ppAction    :: FilePath -> FilePath -> IO (Maybe CompileError)
    -- ^ an function which takes a source path and a destination path and
    -- returns an action which performs the compilation
    }

-- | Yes, there's a bit of redundancy here...
newtype PPs = PPs { unPPs :: M.Map String PP }

noPPs :: PPs
noPPs = PPs M.empty

supportedBy :: PPs -> String -> Bool
supportedBy pps = flip M.member (unPPs pps)

supportedExts :: PPs -> [String]
supportedExts = M.keys . unPPs

insertPP :: PP -> PPs -> PPs
insertPP pp = PPs . M.insert (ppExtension pp) pp . unPPs

lookupPP :: String -> PPs -> Maybe PP
lookupPP ext = M.lookup ext . unPPs

fromList :: [PP] -> PPs
fromList = foldr insertPP noPPs

data Herringbone = Herringbone
    { hbSourceDirs :: [FilePath]
    , hbDestDir    :: FilePath
    , hbPPs        :: PPs
    }

toApplication :: Herringbone -> Application
toApplication hb@(Herringbone { hbDestDir = dest }) =
    staticApp $ (defaultWebAppSettings dest) { ssLookupFile = findAsset hb }

findAsset :: Herringbone -> Pieces -> IO LookupResult
findAsset hb path = do
    assets <- getAssets hb path
    case assets of
        []  -> return LRNotFound
        [x] -> fmap LRFile $ buildAsset hb path x
        xs  -> return . LRFile $ ambiguousSource (map fst xs)

buildAsset :: Herringbone -> Pieces -> (FilePath, [PP]) -> IO File
buildAsset hb pieces (sourcePath, pps) = do
    let destPath = hbDestDir hb </> toFilePath pieces
    let fileName = last pieces
    result <- runPPs pps sourcePath destPath
    either (return . assetCompileError)
           (const (toFile sourcePath destPath fileName))
           result


chain :: Monad m => [a -> m (Either b a)] -> a -> m (Either b a)
chain []     m = return (Right m)
chain (f:fs) m = f m >>= either (return . Left) (chain fs)

runPPs :: [PP] -> FilePath -> FilePath -> IO (Either CompileError ())
runPPs pps source dest = do
    tmpSource <- openTempFile'
    result <- chain (map runPPinTmpDir pps) tmpSource
    either (return . Left) (fmap Right . moveTo dest) result
    where
    moveTo = flip F.rename
    
-- Given a preprocessor and a file path:
--  * run the preprocessor on the filepath,
--  * write the result to a temporary directory
--  * delete the source file (it's assumed to be a temporary file)
--  * and return the result path.
--
-- If the compilation fails, then the source file is not deleted.
runPPinTmpDir :: PP -> FilePath -> IO (Either CompileError FilePath)
runPPinTmpDir pp source = do
    dest <- openTempFile'
    result <- ppAction pp source dest
    maybe (F.removeFile source >> return (Right dest))
          (return . Left)
          result

-- This is just given to wai-app-static which takes care of serving it.
toFile :: FilePath -- ^ source path
       -> FilePath -- ^ dest path
       -> Piece    -- ^ file name
       -> IO File
toFile source dest name = do
    size  <- F.getSize dest
    mtime <- fmap toEpochTime $ F.getModified source
    let strDest = F.encodeString dest
    return File
        { fileGetSize     = fromIntegral size
        , fileToResponse  = \s h -> responseFile s h strDest Nothing
        , fileName        = name
        , fileGetHash     = return Nothing -- TODO
        , fileGetModified = Just mtime
        }

toEpochTime :: UTCTime -> EpochTime
toEpochTime = fromIntegral . toSecs
    where
    toSecs :: UTCTime -> Int
    toSecs = floor . utcTimeToPOSIXSeconds

assetCompileError :: CompileError -> File
assetCompileError err = fileError (toLazyByteString err) "compile-error.html"

ambiguousSource :: [FilePath] -> File
ambiguousSource sources =
    let body = "<h1>Ambiguous asset source</h1>" <>
                "<p>List of possible asset sources:</p>" <>
                "<ul>" <>
                BL.concat (map (\s -> "<li>" <> toLazyByteString s <> "</li>") sources) <>
                "</ul>"
    in fileError body "error-ambiguous-source.html" 

fileError :: BL.ByteString -> Piece -> File
fileError body name =
    File
        { fileGetSize     = fromIntegral $ BL.length body
        , fileToResponse  = \_ headers -> responseLBS status500 headers body
        , fileName        = name
        , fileGetHash     = return Nothing
        , fileGetModified = Nothing
        }

getAssets :: Herringbone -> Pieces -> IO [(FilePath, [PP])]
getAssets hb path = do
    let sourceDirs = hbSourceDirs hb
    let pps        = hbPPs hb
    assets <- sequence $ map (getAssetsFrom pps path) sourceDirs
    return $ concat assets

getAssetsFrom :: PPs         -- ^ List of preprocessors
              -> Pieces      -- ^ requested path pieces
              -> FilePath    -- ^ Directory to look in
              -> IO [(FilePath, [PP])]
getAssetsFrom _   []     _   = return []
getAssetsFrom pps (x:xs) dir = getAssetsFrom pps xs (dir </> toFilePath x)
getAssetsFrom pps [x] dir    = do
    exists <- F.isDirectory dir
    if exists
        then do contents <- F.listDirectory dir
                return $ getAssetsFrom' pps (T.unpack x) contents
        else return []

-- Given a list of preprocessors, the path of an asset we want to serve, and
-- a list of potential source files, return a list of all the files which could
-- be used as a source for that file, together with the preprocessors which
-- would need to be applied (in the correct order) to preprocess that file.
--
-- For example, given preprocessors for "sass" and "erb", the asset path
-- "style.css", and the following list of potential files:
--
--  "style.css"
--  "style.css.sass"
--  "style.css.sass.erb"
--  "style.css.unrecognised-ext"
--  "javascript.js"
--
-- we should get back:
--
--  [ ("style.css", [])
--  , ("style.css.sass", [sass])
--  , ("style.css.sass.erb", [erb, sass])
--  ]
getAssetsFrom' :: PPs
               -> FilePath      -- ^ Asset to serve
               -> [FilePath]    -- ^ Potential source files
               -> [(FilePath, [PP])]
getAssetsFrom' pps assetPath = catMaybes . map resolve
    where
    resolve :: FilePath -> Maybe (FilePath, PPs)
    resolve fp =
        fmap (\xs -> (fp, xs)) $ (resolvePPs pps assetPath fp)

-- Can we apply a sequence of the given preprocessors to the given source file
-- path to get the given asset? If so, return the list of preprocessors which
-- should be applied to it to make this happen.
resolvePPs :: PPs -> FilePath -> FilePath -> Maybe [PP]
resolvePPs pps assetPath source = do
    exts <- getExtraExtensions source assetPath
    pps  <- sequence $ map (lookupPP pps) exts
    return pps

-- Check if a file path is formed from another file path plus a list of
-- extensions, and if so, return those extensions, in reverse order.
-- Eg:
--  getExtraExtensions "game.js.coffee" "game.js"     == Just ["coffee"]
--  getExtraExtensions "game.js.coffee" "style.css"   == Nothing
--  getExtraExtensions "game.js.coffee.erb" "game.js" == Just ["erb", "coffee"]
getExtraExtensions :: FilePath -> FilePath -> Maybe [String]
getExtraExtensions fpWithExts fp = do
    guard (fp `isPrefixOf` fpWithExts)
    let xs = map reverse . endBy "." . reverse . drop (length fp) $ fpWithExts
    return xs
