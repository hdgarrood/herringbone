herringbone
===========

herringbone is a Haskell library for compiling and serving web assets.  It
aims to make it dead simple to create a 'Network.Wai.Middleware' or
'Network.Wai.Application' which deals with all of your static assets, including
preprocessing for languages like Fay, CoffeeScript, Sass, and LESS.

It takes most of its inspiration from the Ruby library, [Sprockets], hence the
name.

Status
------

Alpha.

How to use it
-------------

Suppose you're building a web application, and you have an `assets` directory,
like this:

```
css/
    normalize.css
    main.sass
js/
    jquery.js
    Main.hs
img/
    kitten.jpg
```

You want CSS, JavaScript, and jpg files to be served as-is, but you want to
compile Sass into CSS, and Haskell into JavaScript (let's say via Fay) before
serving it. Additionally you want to be able to edit any of these files, hit
refresh in your browser, and immediately get the new version. Finally, when
it's time to deploy your application, you want to compile all the assets before
you start servicing requests. Herringbone can handle all of this for you.

Start by creating a `Herringbone`, which holds configuration data, such as the
source directory to search for assets in, the destination directory to place
compiled assets into.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Herringbone
import Web.Herringbone.Preprocessor.Sass (sass)
import Web.Herringbone.Preprocessor.Fay (fay)

hb' = IO Herringbone
hb' = herringbone
    ( setSourceDir "assets"
    . setDestDir   "compiled_assets"
    . setPreprocessors [fay, sass]
    )
```

All assets in Herringbone are referenced by their logical path, which is a
relative path to the asset from the source directory. Logical paths are
represented as `[Text]`, where each element in the list represents a path
segment. So, for example, if you wanted to get `js/jquery.js`, you
could do:

```haskell
example1 :: IO ()
example1 = do
    hb <- hb'
    let path = unsafeMakeLogicalPath ["js", "jquery.js"]
```

(Using `unsafeMakeLogicalPath` is ok in this context; the safe version is only
there to make sure there are no `..` in the path, to prevent people requesting
`../../../../etc/passwd`, for example.)

You can now access assets using `findAsset`:

```haskell
    asset <- findAsset hb path
    print . assetSize $ asset           -- file size, in bytes
    print . assetSourcePath $ asset     -- full source path on disk
    print . assetFilePath $ asset       -- full destination path on disk
    print . assetModifiedTime $ asset   -- modification time
    print =<< assetContent asset        -- the contents of the file
```

But you probably want to use an adapter, like the one for Wai, so that you can
access assets over HTTP:

```haskell
serveAssets :: IO ()
serveAssets = do
    hb <- hb'
    Network.Wai.Handler.Warp.run 3000 (toApplication hb)
```

Preprocessors work on files based on their filenames. Each preprocessor
contains information about the kinds of files it consumes and produces. When a
request comes in for `js/Main.js`, Herringbone will match up the Fay
preprocessor with `js/Main.hs` to serve the correct file.

You can create your own preprocessors; for example, here is one that puts the
date on the second line of a HTML5 Application Cache manifest:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Time
import Data.Monoid
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class
import System.Locale
import Web.Herringbone

myPP :: PP
myPP = PP { ppName = "my preprocessor"
          , ppConsumes = ".manifestWithDate"
          , ppProduces = ".manifest"
          , ppAction = insertDate
          }
    where
    insertDate string = do
        datetime <- liftIO getCurrentTime
        let commentLine = "# " <> showTime datetime
        let result = B.unlines . insertAt2 commentLine . B.lines $ string
        return . Right $ result
    showTime = B.pack . formatTime defaultTimeLocale
                            (dateTimeFmt defaultTimeLocale)
    insertAt2 y (x:xs) = x : y : xs
    insertAt2 _ []     = error "oops"
```

Herringbone defines a data type for preprocessor actions: `PPM`. It has
typeclass instances to allow you to perform arbitrary `IO` actions, and also to
obtain information about the asset and the `Herringbone` that is being used to
compile it.

Once you're ready to deploy your app, use `precompile` to compile all of your
assets in one go:

```haskell
main :: IO ()
main = do
    hb <- hb'
    errors <- precompile hb
    when (not . null $ errors) $ do
        putStrLn "warning: compilation errors:"
        mapM_ (print . show) errors

    runYourApp
```

Alternatively you can use the `herringbone-embed` package to embed all of your
assets into your executable with Template Haskell:

```haskell
import Web.Herringbone.Embed

assets :: [(LogicalPath, ByteString)]
assets = $(embedAssets hb')

main :: IO ()
main = do
    serveYourAppWithAssets assets
```

For more information, go and look at the documentation on [Hackage]!

[Sprockets]: https://github.com/sstephenson/sprockets
[Hackage]: http://hackage.haskell.org/package/herringbone
