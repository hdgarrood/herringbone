{-# LANGUAGE TemplateHaskell #-}
module TestPrecompiledAssets where

import Web.Herringbone
import Data.ByteString (ByteString)
import TestHerringbone (testHB)

precompiledEmbedded :: ([(LogicalPath, AssetError)],
                        [(LogicalPath, ByteString)])
precompiledEmbedded = $(precompileEmbed testHB)
