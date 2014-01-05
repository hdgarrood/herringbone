module Web.Herringbone.Preprocessor.CoffeeScript (
    coffeeScript
) where

import Web.Herringbone
import Web.Herringbone.Preprocessor.StdIO

coffeeScript :: PP
coffeeScript = makeStdIOPP "coffee" "coffee" ["--stdio", "--print"]
