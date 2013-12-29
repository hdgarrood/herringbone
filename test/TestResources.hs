module TestResources where

import Network.Wai.Herringbone

testHB :: Herringbone
testHB = herringbone
    ( addSourceDir "test/resources/assets"
    . setDestDir "test/resources/compiled_assets"
    )
