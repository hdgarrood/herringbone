module PreprocessStdIOSpec where

import Test.Hspec
import SpecHelper

spec :: Spec
spec = do
    it "should use stdIO" $ do
        testWithExpectedResult "hello.txt"
