module WaiAdapterTest where

import Network.Wai
import Network.Wai.Herringbone
import Network.Wai.Handler.Warp
import SpecHelper

app :: Application
app = toApplication testHB

runApp :: IO ()
runApp = do
    putStrLn "running on port 3002..."
    run 3002 app
