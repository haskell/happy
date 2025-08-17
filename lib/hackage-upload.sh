#!/bin/sh
set -e
TOP=$(readlink -f $(dirname $0))

# Some sanity checking for version
CABAL_FILE=$(ls *.cabal)
VERSION=$(grep -E '^version:' "$CABAL_FILE" | awk '{print $2}')
if [ -z "$VERSION" ]; then
  echo "❌ No version found in $CABAL_FILE."
  exit 1
fi
echo "✅ Found happy-lib version: $VERSION"

# Check if ChangeLog.md contains the version string
if grep -q "$VERSION" ChangeLog.md; then
  echo "✅ Version $VERSION is mentioned in ChangeLog.md."
else
  echo "❌ Version $VERSION is NOT mentioned in ChangeLog.md!"
  exit 1
fi

sdist=$(mktemp -d $TOP/dist.XXXXXX)
trap 'rm -rf "$sdist"' EXIT

cabal sdist --builddir=$sdist
cabal upload $1 $sdist/sdist/*.tar.gz

cabal haddock --builddir="$sdist" --haddock-for-hackage --enable-doc --haddock-options=--quickjump

cabal upload -d $1 $sdist/*-docs.tar.gz

echo "✅ happy-lib uploaded"
