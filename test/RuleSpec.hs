module RuleSpec where

import Test.Hspec
import SpecHelper

spec :: Spec
spec = do
    let rule = Rule
        { ruleFrom = "coffee"
        , ruleTo   = "js"
        }

    it "should go forwards" $ do
        assertEqual'
            (Just "application.js")
            (applyForwards rule "application.coffee")

    it "should go backwards" $ do
        assertEqual'
            (Just "application.coffee")
            (applyBackwards rule "application.js")

    it "should work on files with lots of extensions" $ do
        assertEqual'
            (Just "foo.bar.1.js")
            (applyForwards rule <=< applyBackwards rule $ "foo.bar.1.js")
