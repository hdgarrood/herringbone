module BuildAssetSpec where

import Control.Monad
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Filesystem.Path.CurrentOS (FilePath, (</>))
import Prelude hiding (FilePath)
import qualified Filesystem as F
import qualified Data.ByteString as B
import Test.Hspec
import Test.HUnit hiding (path)

import Network.Wai.Herringbone
import SpecHelper

spec :: Spec
spec = do
    let sourceDir  = "test/resources/assets"
    let destDir    = hbDestDir testHB
    let workingDir = hbWorkingDir testHB

    let cleanEverything = clean destDir >> clean workingDir
    let withHooks = ( before (do F.createDirectory True workingDir
                                 cleanEverything)
                    . after cleanEverything
                    )

    withHooks $ do
        context "without preprocessors" $ do
            let logPath = lp "buildAsset.js"

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
            it "should run preprocessors" $ do
                let logPath = lp "buildPreprocess.js"

                Right asset <- findAsset testHB logPath

                assertRanPreprocessor "coffee" (assetFilePath asset)

assertRanPreprocessor :: Text -> FilePath -> Assertion
assertRanPreprocessor ext path = do
    contents <- fmap linesBS $ F.readFile path
    let message = "Ran preprocessor: " <> (T.encodeUtf8 ext)
    assertBool
        ("Preprocessor " ++ T.unpack ext ++ " was not run on " ++ es path)
        (message `elem` contents)
    where
    linesBS = tokenise "\n"
    tokenise x y =
        let (h,t) = B.breakSubstring x y
        in  h : if B.null t
                then []
                else tokenise x (B.drop (B.length x) t)
