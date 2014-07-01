module FindAssetSpec where

import Filesystem.Path.CurrentOS ((</>))
import Prelude hiding (FilePath)
import qualified Filesystem as F
import Test.Hspec
import Test.HUnit hiding (path)

import Web.Herringbone
import SpecHelper

spec :: Spec
spec = do
    let destDir = hbDestDir testHB

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
            testWithExpectedResult "add.js"

    context "when there's a compile error" $ do
        it "should report the error" $ do
            Left result <- findAsset testHB (lp "compileError.txt")
            assertEqual' (AssetCompileError "Oh snap!") result

        it "should not create the output file" $ do
            _ <- findAsset testHB (lp "compileError.css")
            exists <- F.isFile (destDir </> "compileError.css")
            assert (not exists)

    context "when requesting a directory (issue #4)" $ do
        it "should return AssetNotFound" $ do
            Left err <- findAsset testHB (lp "html")
            assertEqual' AssetNotFound err

    context "when two assets map to the same logical path" $ do
        it "should return AmbiguousSources" $ do
            Left err <- findAsset testHB (lp "clash.css")
            case err of
                AmbiguousSources _ -> return ()
                x -> assertFailure $
                    "expected an AmbiguousSources error, got: " ++ show x
