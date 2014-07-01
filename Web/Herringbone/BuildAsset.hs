-- | This module contains functions to build assets (that is, run preprocessing
-- if necessary, and copy to destination directory).
module Web.Herringbone.BuildAsset where

import Control.Monad.Reader
import Data.Maybe
import Data.Time
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem as F
import Prelude hiding (FilePath)

import Web.Herringbone.Types

-- | Build an asset based on a BuildSpec to produce a 'Asset'. This action
-- checks whether the compilation is necessary based on the modified times of
-- the source and destination files.
buildAsset :: Herringbone
           -> BuildSpec
           -> IO (Either CompileError Asset)
buildAsset hb spec@(BuildSpec sourcePath' destPath' pp) = do
    verbosePut hb $ "building from: " ++ show spec
    let sourcePath = hbSourceDir hb </> sourcePath'
    let destPath = hbDestDir hb </> destPath'

    sourceModifiedTime <- F.getModified sourcePath
    compileNeeded <- shouldCompile sourceModifiedTime destPath

    result <- if compileNeeded
                then do
                    verbosePut hb $
                        "compiling asset since source path is newer " ++
                        "than destination file"
                    compileAsset hb sourcePath destPath (maybeToList pp)
                else do
                    verbosePut hb $ "compile not needed; source path " ++
                        "is older than destination path"
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
shouldCompile :: UTCTime -> FilePath -> IO Bool
shouldCompile sourceModifiedTime destPath = do
    exists <- F.isFile destPath
    if not exists
        then return True
        else do
            destModifiedTime <- F.getModified destPath
            return $ sourceModifiedTime > destModifiedTime

-- | Compile the given asset by invoking any preprocessors on the source path,
-- and copying the result to the destination path.
compileAsset :: Herringbone
             -> FilePath -- ^ Source path
             -> FilePath -- ^ Dest path
             -> [PP]     -- ^ List of preprocessors to apply
             -> IO (Either CompileError ())
compileAsset hb sourcePath destPath pps = do
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
        go = \acc f -> acc >>= either (return . Left) f
        z  = return (Right m)
