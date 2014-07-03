module AssertionsHelper where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.List (sort)
import Test.HUnit hiding (path)
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem as F

import LazinessHelper
import HerringboneHelper
import Web.Herringbone

testWithInputs :: String -> (a -> Assertion) -> [a] -> Test
testWithInputs groupName f =
    TestLabel groupName . TestList . map mkTest . zipWith assignName ([1,2..] :: [Int])
    where
        mkTest (name, input) = TestLabel name (TestCase (f input))
        assignName n input   = ("input #" ++ show n, input)

resultsDir :: FilePath
resultsDir = "test/resources/results"

testWithExpectedResult :: Text -> Assertion
testWithExpectedResult logicalPathText = do
    let logicalPath = lp logicalPathText
    let filePath = toFilePath logicalPath
    hb <- testHB
    result <- findAsset hb logicalPath
    either (fail . show)
           (assertResultMatches filePath . assetFilePath)
           result

assertResultMatches :: FilePath -> FilePath -> Assertion
assertResultMatches resultName filePath = do
    let resultPath = resultsDir </> resultName
    assertFileContentsMatch resultPath filePath

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

assertFileContentsIs :: ByteString -> FilePath -> Assertion
assertFileContentsIs string path = do
    fileContents <- F.readFile path
    assertEqual' string fileContents

assertEqual' :: (Eq a, Show a) => a -> a -> Assertion
assertEqual' = assertEqual ""

assertSameElems :: (Eq a, Show a, Ord a) => [a] -> [a] -> Assertion
assertSameElems xs ys = assertEqual' (sort xs) (sort ys)

assertIsRight :: Show a => Either a b -> Assertion
assertIsRight (Right _) = return ()
assertIsRight (Left x)  = assertFailure $
                            "Expected a Right value; got: Left " ++ show x
