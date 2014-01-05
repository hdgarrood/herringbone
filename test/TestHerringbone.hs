module TestHerringbone where

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Monoid
import Network.Wai.Handler.Warp

import Web.Herringbone
import Web.Herringbone.Preprocessor.CoffeeScript

mkMockPP :: Text -> PP
mkMockPP ext = PP { ppExtension = ext
                  , ppAction = \sourceData -> do
                        return . Right $
                            "Preprocessed as: " <> T.encodeUtf8 ext <> "\n" <>
                            sourceData
                  }

pp1 :: PP
pp1 = mkMockPP "pp1"

pp2 :: PP
pp2 = mkMockPP "pp2"

pp3 :: PP
pp3 = mkMockPP "pp3"

failingPP :: PP
failingPP = PP { ppExtension = "fails"
               , ppAction = const . return . Left $ "Oh snap!"
               }

testHB :: Herringbone
testHB = herringbone
    ( addSourceDir  "test/resources/assets"
    . addSourceDir  "test/resources/assets2"
    . setDestDir    "test/resources/compiled_assets"
    . addPreprocessors [pp1, pp2, pp3, failingPP, coffeeScript]
    )

runTestHB :: Int -> IO ()
runTestHB port = run port (toApplication testHB)
