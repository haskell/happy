# Happy

Happy is a parser generator for Haskell 98 (and later).

[![Build Status](https://secure.travis-ci.org/simonmar/happy.svg?branch=master)](http://travis-ci.org/simonmar/happy)

* https://www.haskell.org/happy/
* http://hackage.haskell.org/package/happy

Happy is built using Cabal.  First install GHC, then:
```
  $ cabal install
```
If you obtained the development version from https://github.com/simonmar/happy/,
install via:
```
  $ make sdist && cabal install
```

Complete documentation can be found in the directory 'doc', in
DocBook XML format.  To format the documentation, the DocBook-Tools
suite (see http://wiki.docbook.org/DocBookTools)
provides all the bits & pieces you need.  Alternatively, pre-formatted
documentation is available from Happy's homepage (URL above).

The directory 'examples' contains some example parsers that use Happy.

For information on copying and distributing this program, see the file
LICENSE in this directory.

Bugs should be reported at: https://github.com/simonmar/happy/issues

Happy Parsing!

Simon.
