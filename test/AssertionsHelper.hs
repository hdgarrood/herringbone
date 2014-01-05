module AssertionsHelper where

import Control.Applicative
import Control.Monad
import Data.List (sort)
import Test.HUnit hiding (path)
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem as F

import LazinessHelper

testWithInputs :: String -> (a -> Assertion) -> [a] -> Test
testWithInputs groupName f =
    TestLabel groupName . TestList . map mkTest . zipWith assignName ([1,2..] :: [Int])
    where
        mkTest (name, input) = TestLabel name (TestCase (f input))
        assignName n input   = ("input #" ++ show n, input)

-- Extra assertions
assertFileExists :: FilePath -> Assertion
assertFileExists path = do
    exists <- F.isFile path
    assertBool ("Expected a file to exist: " ++ es path) exists

assertFileContentsMatch :: FilePath -> FilePath -> Assertion
assertFileContentsMatch pathA pathB = do
    matches <- (==) <$> F.readFile pathA <*> F.readFile pathB
    assertBool ("expected file contents to match\n" ++
                "this file:       " ++ es pathA ++ "\n" ++
                "versus this one: " ++ es pathB ++ "\n") matches

assertEqual' :: (Eq a, Show a) => a -> a -> Assertion
assertEqual' = assertEqual ""

assertSameElems :: (Eq a, Show a, Ord a) => [a] -> [a] -> Assertion
assertSameElems xs ys = assertEqual' (sort xs) (sort ys)

assertIsRight :: Show a => Either a b -> Assertion
assertIsRight (Right _) = return ()
assertIsRight (Left x)  = assertFailure $
                            "Expected a Right value; got: Left " ++ show x
