-- | This module contains functions to build assets (that is, run preprocessing
-- if necessary, and copy to destination directory).
module Web.Herringbone.BuildAsset where

import Data.Time
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem as F
import Prelude hiding (FilePath)

import Web.Herringbone.Types

-- | Build an asset to produce a 'Asset'. This action checks whether the
-- compilation is necessary based on the modified times of the source and
-- destination files.
buildAsset :: Herringbone
           -> LogicalPath -- ^ Logical path of asset to build
           -> FilePath    -- ^ Source file path
           -> [PP]        -- ^ List of preprocessors to run
           -> IO (Either CompileError Asset)
buildAsset hb logPath sourcePath pps = do
    let destPath = hbDestDir hb </> toFilePath logPath

    sourceModifiedTime <- F.getModified sourcePath
    compileNeeded <- shouldCompile sourceModifiedTime destPath

    result <- if compileNeeded
                then compileAsset sourcePath destPath pps
                else return $ Right ()

    either (return . Left)
           (\_ -> do size <- F.getSize destPath
                     return . Right $ Asset
                                        size
                                        sourcePath
                                        destPath
                                        logPath
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
compileAsset :: FilePath -- ^ Source path
             -> FilePath -- ^ Dest path
             -> [PP]     -- ^ List of preprocessors to apply
             -> IO (Either CompileError ())
compileAsset sourcePath destPath pps = do
    sourceData <- F.readFile sourcePath

    result <- chainEither (map ppAction pps) sourceData
    either (return . Left)
           (\resultData -> do F.writeFile destPath resultData
                              return (Right ()))
           result

chainEither :: Monad m => [a -> m (Either b a)] -> a -> m (Either b a)
chainEither fs m = foldl go z fs
    where
        go = \acc f -> acc >>= either (return . Left) f
        z  = return (Right m)
