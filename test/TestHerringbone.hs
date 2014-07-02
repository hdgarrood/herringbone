module TestHerringbone where

import Control.Monad
import Network.Wai
import Network.Wai.Handler.Warp

import Web.Herringbone
import Web.Herringbone.Preprocessor.StdIO
import Web.Herringbone.Preprocessor.CoffeeScript
import Web.Herringbone.Preprocessor.Sass

failingPP :: PP
failingPP = PP { ppSpec = PPSpec "failing pp" "fails" "txt"
               , ppAction = const . return . Left $ "Oh snap!"
               }

sed :: PP
sed = makeStdIOPP spec "sed" ["-e", "s/e/u/"]
    where
    spec = PPSpec "sed" "sed" "txt"

newCoffee :: PP
newCoffee = PP { ppSpec = PPSpec "CoffeeScript, changed" "coffee" "js"
               , ppAction = const . return . Right $ "changed"
               }

unsafeFromEither :: (Show a, Show b) => Either a b -> b
unsafeFromEither (Right x) = x
unsafeFromEither x = error $ "unsafeFromEither: " ++ show x

testHerringboneSettings :: HerringboneSettings
testHerringboneSettings = unsafeFromEither $ makeSettings
    (   setSourceDir  "test/resources/assets"
    >=> setDestDir    "test/resources/compiled_assets"
    >=> setPreprocessors [ failingPP
                         , coffeeScript
                         , sass
                         , scss
                         , sed
                         ]
    )

testHB :: IO Herringbone
testHB = initHerringbone testHerringboneSettings

testHBVerbose :: IO Herringbone
testHBVerbose = initHerringbone . unsafeFromEither $
    setVerbose testHerringboneSettings

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
