module HerringboneSpec where

import Filesystem.Path.CurrentOS ((</>))
import Prelude hiding (FilePath)
import qualified Filesystem as F
import Test.Hspec
import Test.HUnit hiding (path)

import Web.Herringbone
import Web.Herringbone.Internal.Utils
import SpecHelper

spec :: Spec
spec = do
    let destDir = settingsDestDir testHerringboneSettings
    before (cleanDir destDir) $ do
        context "without preprocessors" $ do
            let logPath = lp "buildNoPreprocessors.js"

            it "should copy a source file to the destination directory" $ do
                hb <- testHB
                asset' <- findAsset hb logPath

                assertIsRight asset'
                let Right asset = asset'
                assertFileExists (assetFilePath asset)
                assertFileContentsMatch (assetSourcePath asset)
                                        (assetFilePath asset)

            it "should get the modification time of the source file" $ do
                hb <- testHB
                Right asset <- findAsset hb logPath
                sourceMTime <- F.getModified (assetSourcePath asset)

                assertEqual' sourceMTime (assetModifiedTime asset)

            it "should not compile unless necessary" $ do
                hb <- testHB
                Right asset  <- findAsset hb logPath
                mTime        <- F.getModified (assetFilePath asset)
                Right asset' <- findAsset hb logPath
                mTime'       <- F.getModified (assetFilePath asset')

                assertEqual' mTime mTime'

        context "with preprocessors" $ do
            it "should run a single preprocessor" $ do
                requiresExecutable "coffee"

                testWithExpectedResult "add.js"

            it "should not create the output file" $ do
                hb <- testHB
                _ <- findAsset hb (lp "compileError.txt")
                exists <- F.isFile (destDir </> "compileError.txt")
                assert (not exists)

        context "when requesting a non-existent asset" $ do
            it "should return AssetNotFound" $ do
                hb <- testHB
                Left res <- findAsset hb (lp "doesnt-exist")
                assertEqual' res AssetNotFound

        context "when requesting a directory (issue #4)" $ do
            it "should return AssetNotFound" $ do
                hb <- testHB
                Left err <- findAsset hb (lp "html")
                assertEqual' AssetNotFound err

        context "with assets in subdirectories" $ do
            it "should compile them" $ do
                testWithExpectedResult "sub/sub.txt"

        context "when replacing a preprocessor" $ do
            it "should recompile the asset" $ do
                requiresExecutable "coffee"

                testWithExpectedResult "add.js"

                hb' <- initHerringbone . setPreprocessors [newCoffee] $
                        testHerringboneSettings
                Right asset <- findAsset hb' (lp "add.js")
                assertFileContentsIs "changed" (assetFilePath asset)

        context "when precompiling assets" $ do
            it "should get them all" $ do
                hb <- testHB
                sources <- getFilesRecursiveRelative (hbSourceDir hb)
                _       <- precompile hb
                dests   <- getFilesRecursiveRelative (hbDestDir hb)

                assertGotAllAssets sources dests

        context "when embedding assets" $ do
            it "should get them all" $ do
                hb <- testHB
                sources <- getFilesRecursiveRelative (hbSourceDir hb)

                assertGotAllAssets sources embeddedAssets

        context "with StdIO preprocessors" $ do
            it "should use stdIO" $ do
                testWithExpectedResult "hello.txt"

            it "should use scss" $ do
                requiresExecutable "scss"
                testWithExpectedResult "blue.css"

            it "should use sass" $ do
                requiresExecutable "sass"
                testWithExpectedResult "red.css"
        
        context "with the error:" $ do
            context "two assets mapping to the same logical path" $ do
                it "should return AmbiguousSources" $ do
                    hb <- testHBFailing
                    Left err <- findAsset hb (lp "clash.css")
                    case err of
                        AmbiguousSources _ -> return ()
                        x -> assertFailure $
                            "expected an AmbiguousSources error, got: "
                            ++ show x

            context "asset compilation error" $ do
                it "should report the error" $ do
                    hb <- testHBFailing
                    Left result <- findAsset hb (lp "compileError.txt")
                    assertEqual' (AssetCompileError "Oh snap!") result

