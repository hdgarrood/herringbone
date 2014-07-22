{- |
herringbone is a Haskell library for compiling and serving web assets.
It aims to make it dead simple to create a 'Network.Wai.Middleware' or
'Network.Wai.Application' which deals with all of your static assets, including
preprocessing for languages like Fay, CoffeeScript, Sass, and LESS.

It takes most of its inspiration from the Ruby library,
<https://github.com/sstephenson/sprockets Sprockets>, hence the name.

Example:

> import Web.Herringbone
>
> fay, sass :: PP
>
> hb = Herringbone
> hb = herringbone
>     ( addSourceDir "assets"
>     . setDestDir   "compiled_assets"
>     . addPreprocessors [fay, sass]
>     )
>
> -- You can now access assets programmatically
> asset <- findAsset hb (fromJust . makeLogicalPath $ ["application.js"])
>
> -- Or serve them with a Wai application
> app = toApplication hb
-}
module Web.Herringbone (
    -- * Creating a Herringbone
    Herringbone,
    hbSourceDir,
    hbDestDir,
    hbPPs,
    hbVerbose,
    HerringboneSettings(..),
    module Web.Herringbone.Internal.Configuration,
    -- * Assets
    LogicalPath,
    makeLogicalPath,
    unsafeMakeLogicalPath,
    fromLogicalPath,
    toFilePath,
    Asset(..),
    findAsset,
    -- * Preprocessors
    PP(..),
    PPs,
    AssetError(..),
    CompileError,
    PPReader(..),
    PPM,
) where

import Web.Herringbone.Internal.Configuration
import Web.Herringbone.Internal.Types
import Web.Herringbone.Internal.FindAsset
