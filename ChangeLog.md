## Unreleased - 1.21.0

- Split `happy` into several packages (happy, happy-core, happy-frontend, happy-middleend, happy-backend, happy-tests).

## 1.20.0

* Fix #121: the -i flag produces an .info file even if the `%expect`
  pragma is violated
* Fix #131: qualify uses of Prelude functions in generated code
* Fix #161: drop fewer parse items when generating an .info file
* Introduce the `%shift` directive to resolve shift/reduce conflicts
  explicitly, useful in conjunction with `%expect 0`
* Remove the deprecated build configuration flag `small_base`

## 1.19.12

* Fix for building with GHC 8.8.x
* Move custom Setup preprocessing steps into a separate executable, like
  Alex

## 1.19.11

* Fix for building with GHC 8.6.x

## 1.19.10

* Fix polymorphic (rank-n) non-terminals
* Fix for GHC 8.8.1


## 1.19.9

* Fix cabal warnings
* Bump upper bounds
* Fix build with GHC 8.4.1-alpha

## 1.19.8

* Fix issue #94 (some grammars don't compile due to new type signatures
  introduced to allow overloading to be used)

## 1.19.7

* Fix missing test suite files in the sdist

## 1.19.6

* Manually generate Parser.hs using Makefile before sdist, to fix
  bootstrapping problems with cabal sandboxes & new-build
* Documentation fixes
* Fixed GLR support
* New option `-p`/`--pretty` prints the grammar rules (only) to a file
* Added generation of additional type signatures to enable use of
  typeclasses in monadic parsers.

## 1.19.5

* Fixes for GHC 7.10
* Code cleanups (thanks Index Int <vlad.z.4096@gmail.com>)

## 1.19.4

* Fix for GHC 7.10 (Applicative/Monad, #19, #21)

## 1.19.3

* Fix for GHC 7.2 (#16)

## 1.19.2

* Fixes for clang (XCode 5)

## 1.19.1

* Repackaged to build with GHC 7.7+

## 1.19

* Necessary changes to work with GHC 7.8

## 1.18.10

* Fix build with GHC 7.6

## 1.18.8

* Fix a packaging bug (cabal-install-0.10.2 didn't put the
  Happy-generated files in the sdist)

## 1.18.7

* Fix a bug in error handling when using `%monad` without `%lexer`

## 1.18

* New feature: EBNF-style paramterized macros, thanks to
  Iavor Diatchki.
* Works with Cabal 1.2, 1.4 and 1.6
* A few minor bugfixes

## 1.17

* Cabal 1.2 is required
* Works with upcoming GHC 6.8.1
* Fix the `parE` bug (poor error message for errors in the grammar)
* Some performance improvements to Happy itself

## 1.16

* Switch to a Cabal build system: you need a recent version of Cabal
  (1.1.6 or later).  If you have GHC 6.4.2, then you need to upgrade
  Cabal before building Happy.  GHC 6.6 is fine.

* New `%error` directive

* New production forms: `{%% .. }` and `{%^ .. }`

* Added Attribute Grammar support, by Robert Dockins

## 1.15

* New `%expect` directive
* The list of tokens passed to happyError now includes the current
  token (not `%lexer`).
* Added support for ambiguous grammars via Generalized LR parsing
* Added `%partial` to indicate a parser that can return a result
  before EOF is reached.

## 1.14

* New meta-variable `$>` represents the rightmost token.

* Happy's OPTIONS pragma is merged with an existing one in
  the grammar file, if any.

## 1.13

* Support for newer versions of GHC (>= 5.04).

* Addition of an experimental flag: `--strict`.

## 1.11

* Tokens no longer have a default precedence --- if you 
  want a token to have a precedence, you have to declare it.

* Bugfix to templates for GHC on 64-bit platforms.

## 1.10

* Bugfixes, and minor performance improvements,

* Most of the examples work again.

## 1.9

* A grammar may now contain several entry points, allowing
  several parsers to share parts of the grammar.

* Some bugfixes.

## 1.8

* Parser table compression, and more efficient table encoding when used
  with GHC.  Large grammars can now be compiled in much less time/space
  than before using GHC.

* Yacc-style operator precedence, thanks to patches from Hermann
  Oliveira Rodrigues <hermann@dcc.ufmg.br> and Josef Svenningsson
  <josefs@cs.chalmers.se>.

* A debug option which causes the generated parser to print tracing
  information at each step during parsing.

## 1.6

* Now written in, and generates, Haskell 98.

* Several bug fixes.

* A new option, `-c`, generates parsers that use GHC's unsafeCoerce#
  primitive to speed up parsing and cut down the binary size.  The `-c`
  option can only be used with the -g (GHC extensions) option.

* Parsers generated with the -g option will compile to smaller binaries
  now --- some sources of parser-bloat were identified and squished.

* Happy has a new Open Source license, based on the BSD license.

* A sample Haskell parser using Happy is included.

## 1.5

* Many bug fixes to the error recovery support, found by experimenting
  with the Haskell grammar and layout.

* Happy is about 5 times faster on large examples, due to some
  changes in the LALR(1) algorithms.

## 1.2

* Supports Haskell 1.4

* Lots of bugs fixed

* Performance: the parser generator is at least 20% faster, and
  generated parsers should be faster due to the replacement of a
  data type with a newtype.

* Simple error recovery: designed to be enough to implement the
  Haskell layout rule.

* Revamped monad support: the monad can now be threaded through
  the lexer, enabling passing of state between the parser and the
  lexer (handy for the Haskell layout rule), and consistent error
  handling.

* The `%newline` feature is removed, the same effect can be achieved
  using the new monad support.

## 0.9:

* Happy should be much faster than before.

* Generated parsers will be 5-10% smaller.

* Happy now compiles with ghc-0.26.

* Support for monadic parsers via `%monad` (see the documentation).

* New syntax: previously

  ```haskell
  f :: { <type> }
  f : ...
    | ...
    etc.
  ```

  can now be written

  ```haskell
  f :: { <type> }
    : ...
    | ...
    etc.
  ```

  (i.e. omit the extra `f`.  It was always ignored anyway :-)

* Miscellaneous bug fixes.
