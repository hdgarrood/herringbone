{-# LANGUAGE TemplateHaskell #-}
module EmbeddedAssetsHelper where

import Web.Herringbone
import Web.Herringbone.Embed
import Data.ByteString (ByteString)
import HerringboneHelper (testHB)

embeddedAssets :: [(LogicalPath, ByteString)]
embeddedAssets = $(embedAssets testHB)
