# Happy

[![Build Status](https://secure.travis-ci.org/simonmar/happy.svg?branch=master)](http://travis-ci.org/simonmar/happy)

Happy is a parser generator for Haskell 98 (and later).

* https://www.haskell.org/happy/
* http://hackage.haskell.org/package/happy

## Build Instructions

Happy is built using Cabal.  First install GHC, then:
```
  $ cabal install happy
```
If you don't have a local version of `happy` (yet), you can also install a non-bootstrapped version via:
```
  $ cabal install happy -f -bootstrap
```

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
