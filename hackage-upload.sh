#!/bin/sh
set -e
TOP=$(readlink -f $(dirname $0))

# Some sanity checking for version
CABAL_FILE=$(ls *.cabal)
VERSION=$(grep -E '^version:' "$CABAL_FILE" | awk '{print $2}')
if [ -z "$VERSION" ]; then
  echo "No version found in $CABAL_FILE."
  exit 1
fi
echo "Found version: $VERSION"

# Ensure that we depend on the proper version of happy-lib
if grep -Eq "(^| )happy-lib[[:space:]]*==[[:space:]]*$VERSION" "$CABAL_FILE"; then
  echo "happy-lib dependency is correctly listed with version $VERSION in $CABAL_FILE."
else
  echo "happy-lib dependency with version $VERSION is NOT listed in $CABAL_FILE!"
  exit 1
fi

sdist=$(mktemp -d $TOP/dist.XXXXXX)
trap 'rm -rf "$sdist"' EXIT

cabal sdist --builddir=$sdist
cabal upload $1 $sdist/sdist/*.tar.gz
