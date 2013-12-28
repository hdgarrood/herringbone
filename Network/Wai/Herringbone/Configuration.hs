module Network.Wai.Herringbone.Configuration where

import Network.Wai.Herringbone.Types
import Filesystem.Path.CurrentOS (FilePath)
import Prelude hiding (FilePath)

type ConfigBuilder = Herringbone -> Herringbone

defaultHerringbone :: Herringbone
defaultHerringbone = Herringbone
    { hbSourceDirs = []
    , hbDestDir    = ""
    , hbPPs        = noPPs
    }

addSourceDir :: FilePath -> ConfigBuilder
addSourceDir dir hb = hb { hbSourceDirs = dir : hbSourceDirs hb }

setDestDir :: FilePath -> ConfigBuilder
setDestDir dir hb = hb { hbDestDir = dir }

addPreprocessors :: [PP] -> ConfigBuilder
addPreprocessors ppList hb = hb { hbPPs = insertAllPPs ppList (hbPPs hb) }

herringbone :: ConfigBuilder -> Herringbone
herringbone builder = builder defaultHerringbone
