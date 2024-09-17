#!/bin/sh
set -e
TOP=$(readlink -f $(dirname $0))

sdist=$(mktemp -d $TOP/dist.XXXXXX)
trap 'rm -rf "$sdist"' EXIT

cabal sdist --builddir=$sdist
cabal upload $1 $sdist/sdist/*.tar.gz

cabal haddock --builddir="$sdist" --haddock-for-hackage --enable-doc --haddock-options=--quickjump

cabal upload -d $1 $sdist/*-docs.tar.gz
