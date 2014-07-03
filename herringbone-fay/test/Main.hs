module Main where

import Data.Default
import Network.Wai
import Network.Wai.Handler.Warp
import Web.Herringbone
import Web.Herringbone.Preprocessor.Fay

fay :: PP
fay = makeFayPP def

hb :: Herringbone
hb = herringbone
    ( addSourceDir "test/resources/assets"
    . setDestDir   "test/resources/compiled_assets"
    . addPreprocessors [fay]
    )

app :: Application
app = toApplication hb

main :: IO ()
main = do
    putStrLn "herringbone-fay test server: starting on port 3000..."
    run 3000 app
