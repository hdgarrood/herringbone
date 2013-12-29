module TestResources where

import Data.Text (Text)
import Network.Wai.Herringbone

mkMockPP :: Text -> PP
mkMockPP ext = PP { ppExtension = ext
                  , ppAction = \_ _ -> return Nothing }

coffee :: PP
coffee = mkMockPP "coffee"

erb :: PP
erb = mkMockPP "erb"

coffeeAndErb :: PPs
coffeeAndErb = fromList [coffee, erb]

testHB :: Herringbone
testHB = herringbone
    ( addSourceDir "test/resources/assets"
    . addSourceDir "test/resources/assets2"
    . setDestDir "test/resources/compiled_assets"
    . addPreprocessors [coffee, erb]
    )
