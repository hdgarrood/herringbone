module Web.Herringbone.Preprocessor.Sass (
    sass,
    scss
) where

import Web.Herringbone
import Web.Herringbone.Preprocessor.StdIO

sass :: PP
sass = makeStdIOPP "sass" "sass" ["--stdin"]

scss :: PP
scss = makeStdIOPP "scss" "sass" ["--stdin", "--scss"]
