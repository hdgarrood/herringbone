module LazinessHelper where

import Web.Herringbone
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as F

es :: FilePath -> String
es = F.encodeString

lp :: Text -> LogicalPath
lp = unsafeMakeLogicalPath . T.splitOn "/"
