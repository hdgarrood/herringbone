module SpecHelper where

import Control.Applicative
import Control.Monad
import Data.List (sort)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Test.HUnit hiding (path)
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F

import Network.Wai.Herringbone

mkMockPP :: Text -> PP
mkMockPP ext = PP { ppExtension = ext
                  , ppAction = \sourceData -> do
                        putStrLn $ "preprocessing: " ++ show ext
                        return . Right $
                            "Preprocessed as: " <> T.encodeUtf8 ext <> "\n" <>
                            sourceData
                  }

coffee :: PP
coffee = mkMockPP "coffee"

erb :: PP
erb = mkMockPP "erb"

sass :: PP
sass = mkMockPP "sass"

coffeeAndErb :: PPs
coffeeAndErb = fromList [coffee, erb]

testHB :: Herringbone
testHB = herringbone
    ( addSourceDir  "test/resources/assets"
    . addSourceDir  "test/resources/assets2"
    . setDestDir    "test/resources/compiled_assets"
    . addPreprocessors [coffee, erb, sass]
    )

testWithInputs :: String -> (a -> Assertion) -> [a] -> Test
testWithInputs groupName f =
    TestLabel groupName . TestList . map mkTest . zipWith assignName ([1,2..] :: [Int])
    where
        mkTest (name, input) = TestLabel name (TestCase (f input))
        assignName n input   = ("input #" ++ show n, input)

clean :: FilePath -> IO ()
clean dir = do
    items <- F.listDirectory dir
    forM_ items $ \item -> do
        isDir <- F.isDirectory item
        if isDir
            then F.removeTree item
            else F.removeFile item

-- Laziness
es :: FilePath -> String
es = F.encodeString

lp :: Text -> LogicalPath
lp = unsafeMakeLogicalPath . T.splitOn "/"

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
