module Web.Herringbone.Configuration where

import Data.Time.Clock (getCurrentTime)
import Web.Herringbone.Types
import Filesystem.Path.CurrentOS (FilePath)
import Prelude hiding (FilePath)

-- | For convenience.
herringbone :: ConfigBuilder -> Either String (IO Herringbone)
herringbone builder = fmap initHerringbone $ makeSettings builder

-- | Creates a 'HerringboneSettings' instance.
makeSettings :: ConfigBuilder -> Either String HerringboneSettings
makeSettings builder = builder defaultSettings

-- | Sets up internal state, and returns the Herringbone, ready to be used.
initHerringbone :: HerringboneSettings -> IO Herringbone
initHerringbone settings = do
    time <- getCurrentTime
    return $ Herringbone
        { herringboneStartTime = time
        , herringboneSettings = settings
        }

-- | Adds a directory to the list of source directories.
setSourceDir :: FilePath -> ConfigBuilder
setSourceDir dir hb = Right $ hb { settingsSourceDir = dir }

-- | Sets the destination directory. Note that this will overwrite the
-- destination directory if one is already set.
setDestDir :: FilePath -> ConfigBuilder
setDestDir dir hb = Right $ hb { settingsDestDir = dir }

-- | Set the preprocessor collection to the given list of preprocessors
setPreprocessors :: [PP] -> ConfigBuilder
setPreprocessors ppList hb = case insertAllPPs ppList noPPs of
    Just pps -> Right $ hb { settingsPPs = pps }
    Nothing  -> Left "Couldn't add all preprocessors."

-- | Displays detailed log information during requests. Useful for debugging.
setVerbose :: ConfigBuilder
setVerbose conf = Right $ conf { settingsVerbose = True }

defaultSettings :: HerringboneSettings
defaultSettings = HerringboneSettings
    { settingsSourceDir  = "."
    , settingsDestDir    = "_compiled_assets"
    , settingsPPs        = noPPs
    , settingsVerbose    = False
    }
