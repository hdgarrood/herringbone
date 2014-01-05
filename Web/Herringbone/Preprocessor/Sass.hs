module Web.Herringbone.Preprocessor.Sass (
    sass,
    scss
) where

import Web.Herringbone
import Web.Herringbone.Preprocessor.StdIO

-- | A preprocessor for the sass mode of Sass.
sass :: PP
sass = makeStdIOPP "sass" "sass" ["--stdin"]

-- | A preprocessor for the scss mode of Sass.
scss :: PP
scss = makeStdIOPP "scss" "sass" ["--stdin", "--scss"]
