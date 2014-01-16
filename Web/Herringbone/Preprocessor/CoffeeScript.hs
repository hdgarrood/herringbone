module Web.Herringbone.Preprocessor.CoffeeScript (
    coffeeScript
) where

import Web.Herringbone
import Web.Herringbone.Preprocessor.StdIO

coffeeScript :: PP
coffeeScript = makeStdIOPP spec "coffee" ["--stdio", "--print"]
    where
    spec = PPSpec
        { ppName     = "CoffeeScript"
        , ppConsumes = "coffee"
        , ppProduces = "js"
        }
