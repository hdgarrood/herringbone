module Web.Herringbone.Internal.Configuration where

import Data.Time.Clock (getCurrentTime)
import Filesystem.Path.CurrentOS (FilePath)
import Prelude hiding (FilePath)

import Web.Herringbone.Internal.Types

-- | For convenience.
herringbone :: ConfigBuilder -> IO Herringbone
herringbone = initHerringbone . makeSettings

-- | Creates a 'HerringboneSettings' instance from a 'ConfigBuilder'.
-- This just applies the config builder to the default settings:
--
-- > makeSettings builder = builder defaultSettings
makeSettings :: ConfigBuilder -> HerringboneSettings
makeSettings builder = builder defaultSettings

-- | Sets up internal state, and returns a Herringbone, ready to be used.
initHerringbone :: HerringboneSettings -> IO Herringbone
initHerringbone settings = do
    time <- getCurrentTime
    return Herringbone
        { herringboneStartTime = time
        , herringboneSettings = settings
        }

-- | Adds a directory to the list of source directories.
setSourceDir :: FilePath -> ConfigBuilder
setSourceDir dir settings =
    settings { settingsSourceDir = dir }

-- | Sets the destination directory. Note that this will overwrite the
-- destination directory if one is already set.
setDestDir :: FilePath -> ConfigBuilder
setDestDir dir settings =
    settings { settingsDestDir = dir }

-- | Set the preprocessor collection to the given list of preprocessors
setPreprocessors :: [PP] -> ConfigBuilder
setPreprocessors ppList settings =
    settings { settingsPPs = fromList ppList }

-- | Add a preprocessor to the 'HerringboneSettings'
addPreprocessor :: PP -> ConfigBuilder
addPreprocessor pp settings =
    settings { settingsPPs = insertPP pp (settingsPPs settings) }

-- | Displays detailed log information during requests. Useful for debugging.
setVerbose :: ConfigBuilder
setVerbose settings =
    settings { settingsVerbose = True }

defaultSettings :: HerringboneSettings
defaultSettings = HerringboneSettings
    { settingsSourceDir  = "."
    , settingsDestDir    = "compiled_assets"
    , settingsPPs        = emptyPPs
    , settingsVerbose    = False
    }
