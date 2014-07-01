module TestHerringbone where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Monoid
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

unsafeFromEither :: (Show a, Show b) => Either a b -> b
unsafeFromEither (Right x) = x
unsafeFromEither x = error $ "unsafeFromEither: " ++ show x

testHB :: Herringbone
testHB = unsafeFromEither $ herringbone
    (   setSourceDir  "test/resources/assets"
    >=> setDestDir    "test/resources/compiled_assets"
    >=> addPreprocessors [ failingPP
                         , coffeeScript
                         , sass
                         , scss
                         , sed
                         ]
    )


testHBVerbose :: Herringbone
testHBVerbose = unsafeFromEither $ setVerbose testHB

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
