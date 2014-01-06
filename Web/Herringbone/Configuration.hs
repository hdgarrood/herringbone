module Web.Herringbone.Configuration where

import Web.Herringbone.Types
import Filesystem.Path.CurrentOS (FilePath)
import Prelude hiding (FilePath)

-- | Preferred way of creating 'Herringbone' instances.
herringbone :: ConfigBuilder -> Herringbone
herringbone builder = builder defaultHerringbone

type ConfigBuilder = Herringbone -> Herringbone

-- | Adds a directory to the list of source directories.
addSourceDir :: FilePath -> ConfigBuilder
addSourceDir dir hb = hb { hbSourceDirs = dir : hbSourceDirs hb }

-- | Sets the destination directory. Note that this will overwrite the
-- destination directory if one is already set.
setDestDir :: FilePath -> ConfigBuilder
setDestDir dir hb = hb { hbDestDir = dir }

-- | Add the preprocessors in the list to the preprocessor collection.
addPreprocessors :: [PP] -> ConfigBuilder
addPreprocessors ppList hb = hb { hbPPs = insertAllPPs ppList (hbPPs hb) }

-- | Displays detailed (debugging) log information during requests.
setVerbose :: ConfigBuilder
setVerbose conf = conf { hbVerbose = True }

defaultHerringbone :: Herringbone
defaultHerringbone = Herringbone
    { hbSourceDirs = []
    , hbDestDir    = error "herringbone: destination dir must be specified"
    , hbPPs        = noPPs
    , hbVerbose    = False
    }
