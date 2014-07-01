module PreprocessStdIOSpec where

import Test.Hspec
import SpecHelper

spec :: Spec
spec = do
    it "should use stdIO" $ do
        testWithExpectedResult "hello.txt"

    it "should use scss" $ do
        testWithExpectedResult "blue.css"

    it "should use sass" $ do
        testWithExpectedResult "red.css"
