module Main where

import Test.Framework

import TestUtils
import LocateAssetsTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testWithInputs "getExtraExtensions"
        test_getExtraExtensions
        data_getExtraExtensions
    , testWithInputs "resolvePPs"
        test_resolvePPs
        data_resolvePPs
    , testWithInputs "lookupPP"
        test_lookupPP
        data_lookupPP
    , testWithInputs "locateAssets"
        test_locateAssets
        data_locateAssets
    ]
