#!/bin/sh
# Produce an sdist tarball with Happy-generated .hs files in the right place.
set -e
cabal sandbox init
cabal install
cabal sdist
