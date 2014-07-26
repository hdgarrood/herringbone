module Web.Herringbone.Adapter.Wai (toApplication) where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as T
import Data.Time
import Data.Time.Clock.POSIX
import System.Posix.Types (EpochTime)
import Data.Monoid
import Data.List
import Data.Maybe
import Network.Wai
import Network.Wai.Application.Static
import WaiAppStatic.Types
import Network.HTTP.Types
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F

import Web.Herringbone


-- | Convert a 'Herringbone' to a WAI 'Application'.
toApplication :: Herringbone -> Application
toApplication hb@(hbDestDir -> dest) =
    staticApp $ (defaultWebAppSettings dest) { ssLookupFile = lookupFile hb }

lookupFile :: Herringbone -> Pieces -> IO LookupResult
lookupFile hb pieces = do
    asset <- findAsset hb (toLogicalPath pieces)
    either assetErrorToLR bundledAssetToLR asset

    where
    assetErrorToLR = return . go

    go AssetNotFound           = LRNotFound
    go (AssetCompileError err) = LRFile . assetCompileError $ err
    go (AmbiguousSources xs)   = LRFile . ambiguousSources $ xs

    bundledAssetToLR asset = do
        file <- toFile (assetSourcePath asset)
                       (assetFilePath asset)
                       (last pieces)
        return . LRFile $ file

-- WaiAppStatic takes care of directory traversal attacks for us
toLogicalPath :: Pieces -> LogicalPath
toLogicalPath = unsafeMakeLogicalPath . map fromPiece

-- This is just given to wai-app-static which takes care of serving it.
toFile :: FilePath -- ^ source path
       -> FilePath -- ^ dest path
       -> Piece    -- ^ file name
       -> IO File
toFile source dest name = do
    size  <- F.getSize dest
    mtime <- F.getModified source
    let strDest = F.encodeString dest
    return File
        { fileGetSize     = fromIntegral size
        , fileToResponse  = \s h -> responseFile s h strDest Nothing
        , fileName        = name
        , fileGetHash     = return Nothing -- TODO
        , fileGetModified = Just . toEpochTime $ mtime
        }

toEpochTime :: UTCTime -> EpochTime
toEpochTime = fromIntegral . toSecs
    where
    toSecs :: UTCTime -> Int
    toSecs = floor . utcTimeToPOSIXSeconds

assetCompileError :: CompileError -> File
assetCompileError err =
    fileError (BL.fromStrict err) (unsafeToPiece "compile-error.html")

ambiguousSources :: [FilePath] -> File
ambiguousSources sources =
    let toLazyBS = BL.fromStrict . T.encodeUtf8 . F.encode
        htmlListItem item = "<li>" <> toLazyBS item <> "</li>"
        htmlList items = BL.concat (map htmlListItem items)
        body = "<h1>Ambiguous asset source</h1>" <>
                "<p>List of possible asset sources:</p>" <>
                "<ul>" <>
                htmlList sources <>
                "</ul>"
    in fileError body (unsafeToPiece "error-ambiguous-source.html")

fileError :: BL.ByteString -> Piece -> File
fileError body name =
    File
        { fileGetSize     = fromIntegral $ BL.length body
        , fileToResponse  = \_ headers -> responseLBS status500 headers body
        , fileName        = name
        , fileGetHash     = return Nothing
        , fileGetModified = Nothing
        }
