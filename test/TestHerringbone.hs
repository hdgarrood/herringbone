module TestHerringbone where

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Monoid
import Network.Wai
import Network.Wai.Handler.Warp

import Web.Herringbone
import Web.Herringbone.Preprocessor.StdIO
import Web.Herringbone.Preprocessor.CoffeeScript
import Web.Herringbone.Preprocessor.Sass

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

sed :: PP
sed = makeStdIOPP "sed" "sed" ["-e", "s/e/u/"]

testHB :: Herringbone
testHB = herringbone
    ( addSourceDir  "test/resources/assets"
    . addSourceDir  "test/resources/assets2"
    . setDestDir    "test/resources/compiled_assets"
    . addPreprocessors [ pp1
                       , pp2
                       , pp3
                       , failingPP
                       , coffeeScript
                       , sass
                       , scss
                       , sed
                       ]
    )


testHBVerbose :: Herringbone
testHBVerbose = setVerbose testHB

testServerPort :: Int
testServerPort = 3002

runTestHB :: IO ()
runTestHB = runWithMsg
    testServerPort
    "normal"
    (toApplication testHB)

runTestHBVerbose :: IO ()
runTestHBVerbose = runWithMsg
    testServerPort
    "verbose"
    (toApplication testHBVerbose)

runWithMsg :: Int -> String -> Application -> IO ()
runWithMsg port appName app = do
    putStrLn $ "starting " ++ appName ++ " app on port " ++ show port ++ "..."
    run port app
