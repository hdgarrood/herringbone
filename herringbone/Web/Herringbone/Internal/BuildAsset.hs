-- | This module contains functions to build assets (that is, run preprocessing
-- if necessary, and copy to destination directory).
module Web.Herringbone.Internal.BuildAsset where

import Control.Monad.Reader
import Data.Maybe
import Data.Time
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import Prelude hiding (FilePath)

import Web.Herringbone.Internal.Types

-- | Build an asset based on a BuildSpec to produce a 'Asset'. This action
-- checks whether the compilation is necessary based on the modified times of
-- the source and destination files.
buildAsset :: Herringbone
            -> BuildSpec
            -> IO (Either AssetError Asset)
buildAsset hb spec = fmap (mapLeft AssetCompileError) (buildAsset' hb spec)
    where
    mapLeft :: (a -> b) -> Either a r -> Either b r
    mapLeft f (Left x)  = Left $ f x
    mapLeft _ (Right x) = Right x

buildAsset' :: Herringbone
           -> BuildSpec
           -> IO (Either CompileError Asset)
buildAsset' hb (BuildSpec sourcePath' destPath' pp) = do
    verbosePut hb $
        "asset requested: " ++ show sourcePath' ++
        "\n\tto: " ++ show destPath' ++
        maybe "" ("\n\twith preprocessor: " ++) (fmap show pp)

    let sourcePath = hbSourceDir hb </> sourcePath'
    let destPath = hbDestDir hb </> destPath'

    sourceModifiedTime <- F.getModified sourcePath
    compileNeeded <- shouldCompile hb sourceModifiedTime destPath

    result <- if compileNeeded
                then do
                    verbosePut hb "compiling asset..."
                    compileAsset hb sourcePath destPath (maybeToList pp)
                else do
                    verbosePut hb "asset compilation not needed"
                    return $ Right ()

    either (return . Left)
           (\_ -> do size <- F.getSize destPath
                     return . Right $ Asset
                                        size
                                        sourcePath
                                        destPath
                                        sourceModifiedTime)
           result

-- | Should we compile an asset? True if either the asset doesn't exist, or if
-- its modified time is older than the supplied source modification time.
shouldCompile :: Herringbone -> UTCTime -> FilePath -> IO Bool
shouldCompile hb sourceModifiedTime destPath = do
    exists <- F.isFile destPath
    if not exists
        then return True
        else do
            destModifiedTime <- F.getModified destPath
            let changedTime = max sourceModifiedTime (herringboneStartTime hb)
            return $ changedTime > destModifiedTime

-- | Compile the given asset by invoking any preprocessors on the source path,
-- and copying the result to the destination path.
compileAsset :: Herringbone
             -> FilePath -- ^ Source path
             -> FilePath -- ^ Dest path
             -> [PP]     -- ^ List of preprocessors to apply
             -> IO (Either CompileError ())
compileAsset hb sourcePath destPath pps = do
    -- Ensure the destination directory exists
    let destDir = F.directory destPath
    verbosePut hb $ "creating dest dir: " ++ show destDir
    F.createTree destDir

    sourceData <- F.readFile sourcePath
    let computation = chainEither (map ppAction pps) sourceData
    let readerData = PPReader
            { ppReaderHb          = hb
            , ppReaderSourcePath  = sourcePath
            , ppReaderPPs         = pps
            }
    result <- runPPM computation readerData

    either (return . Left)
           (\resultData -> do F.writeFile destPath resultData
                              return (Right ()))
           result

chainEither :: Monad m => [a -> m (Either b a)] -> a -> m (Either b a)
chainEither fs m = foldl go z fs
    where
        go acc f = acc >>= either (return . Left) f
        z  = return (Right m)
