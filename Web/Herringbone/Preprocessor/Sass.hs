module Web.Herringbone.Preprocessor.Sass (
    sass,
    scss
) where

import Web.Herringbone
import Web.Herringbone.Preprocessor.StdIO

-- | A preprocessor for the sass mode of Sass.
sass :: PP
sass = makeStdIOPP spec "sass" ["--stdin"]
    where
    spec = PPSpec
        { ppName     = "Sass (sass mode)"
        , ppConsumes = "sass"
        , ppProduces = "css"
        }

-- | A preprocessor for the scss mode of Sass.
scss :: PP
scss = makeStdIOPP spec "sass" ["--stdin", "--scss"]
    where
    spec = PPSpec
        { ppName     = "Sass (scss mode)"
        , ppConsumes = "scss"
        , ppProduces = "css"
        }
