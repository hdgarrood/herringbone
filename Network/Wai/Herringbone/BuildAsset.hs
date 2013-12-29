-- | This module contains functions to build assets (that is, run preprocessing
-- if necessary, and copy to destination directory).
module Network.Wai.Herringbone.BuildAsset where

import Data.Maybe
import Data.Time
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem as F
import Prelude hiding (FilePath)

import Network.Wai.Herringbone.Types
import Network.Wai.Herringbone.FileSystemUtils

-- | Build an asset to produce a 'BundledAsset'. This action checks whether the
-- compilation is necessary based on the modified times of the source and
-- destination files.
buildAsset :: Herringbone
           -> LogicalPath -- ^ Logical path of asset to build
           -> FilePath    -- ^ Source file path
           -> [PP]        -- ^ List of preprocessors to run
           -> IO (Either CompileError BundledAsset)
buildAsset hb logPath sourcePath pps = do
    let destPath = hbDestDir hb </> toFilePath logPath

    sourceModifiedTime <- F.getModified sourcePath
    compileNeeded <- shouldCompile sourceModifiedTime destPath

    result <- if compileNeeded
                then compileAsset destPath sourcePath pps
                else return $ Right ()

    either (return . Left)
           (\_ -> do size <- F.getSize destPath
                     return . Right $ BundledAsset
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
compileAsset destPath sourcePath pps = do
    tmpSource <- makeTempFile
    F.copyFile sourcePath tmpSource
    result <- chain (map runPPinTmpDir pps) tmpSource
    either (return . Left)
           (\destTmp -> do F.rename destTmp destPath
                           return (Right ()))
           result
    
-- | Given a preprocessor and a file path:
--  * run the preprocessor on the filepath,
--  * write the result to a temporary directory
--  * delete the source file (it's assumed to be a temporary file)
--  * and return the result path.
--
-- If the compilation fails, then the source file is not deleted.
runPPinTmpDir :: PP -> FilePath -> IO (Either CompileError FilePath)
runPPinTmpDir pp source = do
    dest <- makeTempFile
    result <- ppAction pp source dest
    maybe (F.removeFile source >> return (Right dest))
          (return . Left)
          result

chain :: Monad m => [a -> m (Either b a)] -> a -> m (Either b a)
chain []     m = return (Right m)
chain (f:fs) m = f m >>= either (return . Left) (chain fs)
