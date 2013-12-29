module BuildAssetSpec where

import Test.Hspec
import Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem as F
import Prelude hiding (FilePath)

import Network.Wai.Herringbone
import SpecHelper

spec :: Spec
spec = do
    let destDir = "test/resources/compiled_assets"
    after (clean destDir) $ do
        context "without preprocessors" $ do
            let source = "test/resources/assets/test.js"
            let dest   = destDir </> "test.js"

            it "should copy a source file to the destination directory" $ do
                asset <- buildAsset testHB (lp "test.js") source []

                assertIsRight asset
                assertFileExists dest
                assertFileContentsMatch source dest

            it "should not modify the source file" $ do
                _ <- buildAsset testHB (lp "test.js") source []

                assertFileExists source

            it "should get the modification time of the source file" $ do
                sourceMTime <- F.getModified source
                Right asset <- buildAsset testHB (lp "test.js") source []

                assertEqual' sourceMTime (assetModifiedTime asset)
