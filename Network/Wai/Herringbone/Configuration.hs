module Network.Wai.Herringbone.Configuration where

import Network.Wai.Herringbone.Types
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

defaultHerringbone :: Herringbone
defaultHerringbone = Herringbone
    { hbSourceDirs = []
    , hbDestDir    = error "herringbone: destination dir must be specified"
    , hbPPs        = noPPs
    }
