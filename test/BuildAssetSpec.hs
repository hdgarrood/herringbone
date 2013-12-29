module BuildAssetSpec where

import Test.Hspec
import Filesystem.Path.CurrentOS ((</>))
import Prelude hiding (FilePath)

import Network.Wai.Herringbone.BuildAsset
import SpecHelper

spec :: Spec
spec = do
    let destDir = "test/resources/assets"
    after (clean destDir) $ do
        context "without preprocessors" $ do
            it "should copy a source file to the destination directory" $ do
                let source = "test/resources/assets/test.js"
                let dest   = destDir </> "test.js"

                asset <- buildAsset testHB (lp "test.js") source []

                assertIsRight asset
                assertFileExists dest
                assertFileContentsMatch source dest
