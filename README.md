# consuming-parser

[![GitHub](https://img.shields.io/github/v/tag/jumper149/consuming-parser)](https://github.com/jumper149/consuming-parser/tags)
[![Hackage](https://img.shields.io/hackage/v/consuming-parser.svg)](http://hackage.haskell.org/package/consuming-parser)
[![License](https://img.shields.io/github/license/jumper149/consuming-parser)](./LICENSE)

## Incremental Nix Builds
Usually you have to compile all Haskell modules after a change, when building with Nix.
This ensures purity.

But actually GHC is able to tell, whether a module has to be rebuilt or not.
That's why you can use the previous result as an input for a new build.

The previous result is copied to the output directory of GHC.
Now GHC finds the old modules and only recompiles the ones, that actually changed.

Starting with GHC 9.4.2 this change detection works, by checking the hash of the source of the module.
In earlier GHC version this check was done using the timestamps of the files, which is a lot more fragile with nix.

So if have cached a previous build, you can quickly recompile just the changes with nix.
