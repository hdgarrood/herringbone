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

Start by creating a `Herringbone`, which holds configuration data, such as the
source directory to search for assets in, the destination directory to place
compiled assets into.

```haskell
import Web.Herringbone

hb' = IO Herringbone
hb' = herringbone
    ( setSourceDir "assets"
    . setDestDir   "compiled_assets"
    )
```

All assets in Herringbone are referenced by their logical path, which is a
relative path to the asset from the source directory. Herringbone uses a type
synonym `LogicalPath = [Text]`, where each element in the list represents a
path segment. So, for example, if you wanted to get
`javascript/application.js`, you could do:

```haskell
example1 :: IO ()
example1 = do
    hb <- hb'
    let path' = makeLogicalPath ["javascript", "application.js"]
    path = case path' of
        Just p -> p
        Nothing -> fail "invalid path"
```

You can now access assets using `findAsset`:

```
    asset <- findAsset hb path
    print . assetSize $ asset         # file size, in bytes
    print . assetSourcePath $ asset   # full source path on disk
    print . assetFilePath $ asset     # full destination path on disk
    print . assetModifiedTime $ asset # modification time
    print =<< assetContent asset      # the contents of the file
```

But you probably want to use an adapter, like the one for Wai, so that you can
access assets over HTTP:

```
serveAssets :: IO ()
serveAssets = do
    hb <- hb'
    Network.Wai.Handler.Warp.run 3000 (toApplication hb)
```

Herringbone's real utility comes when you want to preprocess files before
serving them. Suppose you are fed up with JavaScript and CSS, and you want to
use CoffeeScript and Sass instead. Add the preprocessors to your `Herringbone`: 

```
import Web.Herringbone.Preprocessor.CoffeeScript (coffeeScript)
import Web.Herringbone.Preprocessor.Sass (sass)

hb2' :: IO Herringbone
hb2' = herringbone
    ( setSourceDir "assets"
    . setDestDir   "compiled_assets"
    . setPreprocessors [coffeeScript, sass]
    )
```

Now suppose you've created `javascript/main.coffee` and `css/styles.sass`; you
will be able access compiled versions of these via the logical paths
`javascript/main.js` and `css/styles.css`.

Using an adapter together with preprocessors is best suited to development,
where after writing a file in your text editor, you simply refresh the page to
get the updated version. In production, you will probably want to do all the
compilation beforehand. Herringbone allows for that too:

```
main :: IO ()
main = do
    hb <- hb'
    errors <- precompile hb
    when (not . null $ errors) $ do
        putStrLn "warning: compilation errors:"
        mapM_ (print . show) errors

    serveYourApp
```

Alternatively you can use the `herringbone-embed` package to embed all of your
assets into your executable with Template Haskell:

```
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
