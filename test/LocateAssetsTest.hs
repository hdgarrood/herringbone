module LocateAssetsTest where

import Test.HUnit hiding (path)
import Data.Text (Text)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import Prelude hiding (FilePath)

import Network.Wai.Herringbone.LocateAssets
import Network.Wai.Herringbone.Types
import TestResources

assertEqual' :: (Eq a, Show a) => a -> a -> Assertion
assertEqual' = assertEqual ""

test_getExtraExtensions :: (FilePath, FilePath, Maybe [Text]) -> Assertion
test_getExtraExtensions (pathWithExts, path, expected) =
    assertEqual' expected (getExtraExtensions pathWithExts path)

data_getExtraExtensions :: [(FilePath, FilePath, Maybe [Text])]
data_getExtraExtensions =
    [ ("game.js"   , "game.js.coffee"     , Just ["coffee"])
    , ("style.css" , "game.js.coffee"     , Nothing)
    , ("game.js"   , "game.js.coffee.erb" , Just ["erb", "coffee"])
    ]

test_resolvePPs :: (PPs, FilePath, FilePath, Maybe [PP]) -> Assertion
test_resolvePPs (pps, assetPath, sourcePath, expected) =
    assertEqual' expected (resolvePPs pps assetPath sourcePath)

mkMockPP :: Text -> PP
mkMockPP ext = PP { ppExtension = ext
                  , ppAction = \_ _ -> return Nothing }

coffee :: PP
coffee = mkMockPP "coffee"

erb :: PP
erb = mkMockPP "erb"

coffeeAndErb :: PPs
coffeeAndErb = fromList [coffee, erb]

data_resolvePPs :: [(PPs, FilePath, FilePath, Maybe [PP])]
data_resolvePPs =
    [ (noPPs        , "test.js"   , "test.js"            , Just [])
    , (noPPs        , "test.js"   , "test.js.coffee"     , Nothing)
    , (coffeeAndErb , "test.js"   , "test.js.coffee"     , Just [coffee])
    , (coffeeAndErb , "test.js"   , "test.js.coffee.erb" , Just [erb, coffee])
    , (coffeeAndErb , "style.css" , "test.js.coffee"     , Nothing)
    ]

test_lookupPP :: (Text, PPs, Maybe PP) -> Assertion
test_lookupPP (ext, pps, expected) =
    assertEqual' expected (lookupPP ext pps)

data_lookupPP :: [(Text, PPs, Maybe PP)]
data_lookupPP =
    [ ("sass" , noPPs        , Nothing)
    , ("sass" , coffeeAndErb , Nothing)
    , ("erb"  , coffeeAndErb , Just erb)
    ]

test_locateAssets :: (LogicalPath, [(FilePath, [PP])]) -> Assertion
test_locateAssets (logPath, expected) = do
    assets <- locateAssets testHB logPath
    assertEqual' expected assets

data_locateAssets :: [(LogicalPath, [(FilePath, [PP])])]
data_locateAssets =
    [ (lp ["test.js"], [(base1 </> "test.js", [])])
    , (lp ["style.css"], [ (base1 </> "style.css", [])
                         , (base1 </> "style.css.erb", [erb])
                         , (base2 </> "style.css", [])
                         ])
    ]
    where
    lp = unsafeMakeLogicalPath
    base1 = "test/resources/assets"
    base2 = "test/resources/assets2"
