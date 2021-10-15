# Happy

[![Build Status](https://github.com/simonmar/happy/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/simonmar/happy/actions)

Happy is a parser generator for Haskell 98 (and later).

* https://www.haskell.org/happy/
* http://hackage.haskell.org/package/happy

## Build Instructions

Happy is normal Cabal-packaged Haskell executable, except for the fact that a
pre-built Happy is *required* to build the full version of Happy, which is the
default.

- If you *do* have an existing Happy executable on the PATH or in the default
  installation location (`~/.cabal/bin` for example), do regular
  ```
  $ cabal build
  ```
  like with any other project.

- If you do *not* have an existing Happy executable, instead do
  ```
  $ cabal build -f -bootstrap
  ```

- If you install that minimial, non-bootstrapped happy
  ```
  $ cabal install -f -bootstrap
  ```
  you can then build normally (with the bootstrap flag enabled).

*We're sorry the bootstrap process is a bit tedious right now; we hope to
improve it in the future. The ideal fix would be to make cabal-installer's
cycle detector to be less pessimistic, per
https://github.com/haskell/cabal/issues/7189, so that the build tool dependency
can be properly expressed and everything works automatically.*

## Documentation & Examples

Complete documentation can be found in the directory 'doc', in
DocBook XML format.  To format the documentation, the DocBook-Tools
suite (see https://github.com/docbook/wiki/wiki/DocBookTools)
provides all the bits & pieces you need.  Alternatively, pre-formatted
documentation is available from Happy's homepage (URL above).

The directory 'examples' contains some example parsers that use Happy.

For information on copying and distributing this program, see the file
LICENSE in this directory.

## Contributing & Reporting Issues

Bugs should be reported at: https://github.com/simonmar/happy/issues

Happy Parsing!

Simon.

## Current Maintainers

- Vladislav Zavialov (@int-index)

- John Ericson (@Ericson2314)

- Simon Marlow (@simonmar)
