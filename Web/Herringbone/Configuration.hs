module Web.Herringbone.Configuration where

import Web.Herringbone.Types
import Filesystem.Path.CurrentOS (FilePath)
import Prelude hiding (FilePath)

-- | Preferred way of creating 'Herringbone' instances.
herringbone :: ConfigBuilder -> Either String Herringbone
herringbone builder = builder defaultHerringbone

type ConfigBuilder = Herringbone -> Either String Herringbone

-- | Adds a directory to the list of source directories.
addSourceDir :: FilePath -> ConfigBuilder
addSourceDir dir hb = Right $ hb { hbSourceDirs = dir : hbSourceDirs hb }

-- | Sets the destination directory. Note that this will overwrite the
-- destination directory if one is already set.
setDestDir :: FilePath -> ConfigBuilder
setDestDir dir hb = Right $ hb { hbDestDir = dir }

-- | Add the preprocessors in the list to the preprocessor collection.
addPreprocessors :: [PP] -> ConfigBuilder
addPreprocessors ppList hb = case insertAllPPs ppList (hbPPs hb) of
    Just pps -> Right $ hb { hbPPs = pps }
    Nothing  -> Left "Couldn't add all preprocessors."

-- | Displays detailed (debugging) log information during requests.
setVerbose :: ConfigBuilder
setVerbose conf = Right $ conf { hbVerbose = True }

defaultHerringbone :: Herringbone
defaultHerringbone = Herringbone
    { hbSourceDirs = []
    , hbDestDir    = error "herringbone: destination dir must be specified"
    , hbPPs        = noPPs
    , hbVerbose    = False
    }
