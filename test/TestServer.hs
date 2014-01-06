module Main where

import TestHerringbone

main :: IO ()
main = do
    let port = 3002 :: Int
    putStrLn $ "starting test server on port " ++ show port ++ "..."
    runTestHBVerbose port
