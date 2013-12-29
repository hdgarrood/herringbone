module LocateAssetsTest where

import Test.HUnit hiding (path)
import Data.Text (Text)
import Filesystem.Path.CurrentOS (FilePath)
import Prelude hiding (FilePath)
import Network.Wai.Herringbone.LocateAssets

import TestUtils

assertEqual' :: (Eq a, Show a) => a -> a -> Assertion
assertEqual' = assertEqual ""

test_getExtraExtensions :: (FilePath, FilePath, Maybe [Text]) -> Assertion
test_getExtraExtensions (pathWithExts, path, expected) =
    assertEqual' expected (getExtraExtensions pathWithExts path)

data_getExtraExtensions :: [(FilePath, FilePath, Maybe [Text])]
data_getExtraExtensions =
    [ ("game.js.coffee"     , "game.js"    , Just ["coffee"])
    , ("game.js.coffee"     , "style.css"  , Nothing)
    , ("game.js.coffee.erb" , "game.js"    , Just ["erb", "coffee"])
    ]
