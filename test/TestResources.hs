module TestResources where

import Network.Wai.Herringbone

testHB :: Herringbone
testHB = herringbone
    ( addSourceDir "test/resources/assets"
    . addSourceDir "test/resources/assets2"
    . setDestDir "test/resources/compiled_assets"
    )
