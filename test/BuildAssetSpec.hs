module BuildAssetSpec where

import Control.Monad
import Data.Text (Text)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import Prelude hiding (FilePath)
import qualified Filesystem as F
import Test.Hspec
import Test.HUnit hiding (path)

import Network.Wai.Herringbone
import SpecHelper

spec :: Spec
spec = do
    let destDir    = hbDestDir testHB

    let withHooks = ( before (clean destDir)
                    . after  (clean destDir)
                    )

    withHooks $ do
        context "without preprocessors" $ do
            let logPath = lp "buildNoPreprocessors.js"

            it "should copy a source file to the destination directory" $ do
                asset' <- findAsset testHB logPath

                assertIsRight asset'
                let Right asset = asset'
                assertFileExists (assetFilePath asset)
                assertFileContentsMatch (assetSourcePath asset)
                                        (assetFilePath asset)

            it "should get the modification time of the source file" $ do
                Right asset <- findAsset testHB logPath
                sourceMTime <- F.getModified (assetSourcePath asset)

                assertEqual' sourceMTime (assetModifiedTime asset)

            it "should not compile unless necessary" $ do
                Right asset  <- findAsset testHB logPath
                mTime        <- F.getModified (assetFilePath asset)
                Right asset' <- findAsset testHB logPath
                mTime'       <- F.getModified (assetFilePath asset')

                assertEqual' mTime mTime'

        context "with preprocessors" $ do
            it "should run a single preprocessor" $ do
                testWithExpectedResult "onePreprocessor.js"

            it "should run preprocessors in the correct order" $ do
                testWithExpectedResult "threePreprocessors.js"

resultsDir :: FilePath
resultsDir = "test/resources/results"

testWithExpectedResult :: Text -> Assertion
testWithExpectedResult logicalPathText = do
    let logicalPath = lp logicalPathText
    let filePath = toFilePath logicalPath
    result <- findAsset testHB logicalPath
    either (fail . show)
           (assertResultMatches filePath . assetFilePath)
           result

assertResultMatches :: FilePath -> FilePath -> Assertion
assertResultMatches resultName filePath = do
    let resultPath = resultsDir </> resultName
    assertFileContentsMatch resultPath filePath
