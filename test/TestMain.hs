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
    ]
