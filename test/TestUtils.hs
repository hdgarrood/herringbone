module TestUtils where

import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFH
import qualified Test.HUnit as HU

testWithInputs :: String -> (a -> HU.Assertion) -> [a] -> TF.Test
testWithInputs groupName f =
    TF.testGroup groupName . map mkTest . zipWith assignName ([1,2..] :: [Int])
    where
        mkTest (name, input) = TFH.testCase name $ f input
        assignName n input   = ("input #" ++ show n, input)
