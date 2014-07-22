module HerringboneHelper where

import Network.Wai
import Network.Wai.Handler.Warp

import Web.Herringbone
import Web.Herringbone.Adapter.Wai
import Web.Herringbone.Preprocessor.StdIO
import Web.Herringbone.Preprocessor.CoffeeScript
import Web.Herringbone.Preprocessor.Sass

failingPP :: PP
failingPP = PP { ppName     = "failing pp"
               , ppConsumes = "fails"
               , ppProduces = "txt"
               , ppAction   = const . return . Left $ "Oh snap!"
               }

sed :: PP
sed = makeStdIOPP "sed: e2u" "sed" "txt" "sed" ["-e", "s/e/u/"]

newCoffee :: PP
newCoffee =
    coffeeScript { ppAction = const . return . Right $ "changed" }

unsafeFromEither :: (Show a, Show b) => Either a b -> b
unsafeFromEither (Right x) = x
unsafeFromEither x = error $ "unsafeFromEither: " ++ show x

testHerringboneSettings :: HerringboneSettings
testHerringboneSettings = makeSettings
    ( setSourceDir  "resources/assets"
    . setDestDir    "resources/compiled_assets"
    . setPreprocessors [ failingPP
                       , coffeeScript
                       , sass
                       , scss
                       , sed
                       ]
    )

testHB :: IO Herringbone
testHB = initHerringbone testHerringboneSettings

testHBVerbose :: IO Herringbone
testHBVerbose = initHerringbone $ setVerbose testHerringboneSettings

testServerPort :: Int
testServerPort = 3002

runTestHB :: IO ()
runTestHB = do
    hb <- testHB
    runWithMsg
        testServerPort "normal" (toApplication hb)

runTestHBVerbose :: IO ()
runTestHBVerbose = do
    hb <- testHBVerbose
    runWithMsg
        testServerPort "normal" (toApplication hb)

runWithMsg :: Int -> String -> Application -> IO ()
runWithMsg port appName app = do
    putStrLn $ "starting " ++ appName ++ " app on port " ++ show port ++ "..."
    run port app

