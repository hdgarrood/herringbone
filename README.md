wai-herringbone
===============

wai-herringbone is a Haskell/Wai library for compiling and serving web assets.
It aims to make it dead simple to create a `Middleware` or `Application` which
deals with all of your static assets, including preprocessing for languages
like Fay, CoffeeScript, Sass, and LESS.

It takes most of its inspiration from the Ruby library, [Sprockets], hence the
name.

Status
------

Pre-alpha. I haven't even written enough code to make this example work yet.

How to use it
-------------

The most important function is `herringbone :: ConfigBuilder -> Herringbone`.
Example use:

```haskell
assets :: Herringbone
assets = herringbone
    ( addSourceDir "assets"
    . setDestDir "_compiled_assets"
    . addPreprocessors [sass, fay]
    . setMode Development
    )
```

From there, you can:

* Convert your `Herringbone` into a wai `Application` or `Middleware`, with
  `toApplication` and `toMiddleware` respectively
* Access assets programmatically with `findAsset :: Herringbone -> AssetPath ->
  IO BundledAsset`

### Defining custom preprocessors

Here's how preprocessors look:

```
type Preprocessor :: FilePath --^ the source file path
                  -> FilePath --^ the destination file path
                  -> IO CompileResult
```

where `data CompileResult = Success | Failure Text`.

To make a custom preprocessor, you provide this function, and add it to the
preprocessor list. Herringbone takes care of the rest.

[Sprockets]: https://github.com/sstephenson/sprockets
