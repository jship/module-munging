# [module-munging][]

[![Build badge][]][build]
[![Version badge][]][version]

## Synopsis

Thin (threadbare), low-tech means of smashing together raw text to create
Haskell modules. This can be useful for code generation, preprocessors, etc.

## Goals

`module-munging` exists primarily to ease building preprocessors. Preprocessors
require smashing together text to make modules, and it gets tiresome quickly
dealing again and again with things like imports, exports, etc. Additionally,
many preprocessors are simple enough that pulling in something like
`haskell-src-exts` is very much overkill. `module-munging` aims to provide as
thin of a wrapper over smashing together text as possible while making the act
of doing so a bit more pleasant.

[module-munging]: https://github.com/jship/module-munging
[Build badge]: https://github.com/jship/module-munging/workflows/CI/badge.svg
[build]: https://github.com/jship/module-munging/actions
[Version badge]: https://img.shields.io/hackage/v/module-munging?color=brightgreen&label=version&logo=haskell
[version]: https://hackage.haskell.org/package/module-munging
[Haddocks]: https://hackage.haskell.org/package/module-munging
