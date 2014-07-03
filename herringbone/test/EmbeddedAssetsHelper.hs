{-# LANGUAGE TemplateHaskell #-}
module EmbeddedAssetsHelper where

import Web.Herringbone
import Data.ByteString (ByteString)
import HerringboneHelper (testHB)

embeddedAssets :: ([(LogicalPath, AssetError)],
                   [(LogicalPath, ByteString)])
embeddedAssets = $(embedAssets testHB)
