wai-herringbone
===============

wai-herringbone is a Haskell/Wai library for compiling and serving web assets.
It aims to make it dead simple to create a `Middleware` which deals with all of
your static assets, including preprocessing for languages like Fay,
CoffeeScript, Sass, and LESS.

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
* Access assets programmatically with `findAsset :: Herringbone -> IO
  BundledAsset`

[Sprockets]: https://github.com/sstephenson/sprockets
