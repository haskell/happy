#!/bin/sh
set -e
TOP=$(readlink -f $(dirname $0))

sdist=$(mktemp -d $TOP/dist.XXXXXX)
trap 'rm -rf "$sdist"' EXIT

cabal sdist --builddir=$sdist
cabal upload $1 $sdist/sdist/*.tar.gz
