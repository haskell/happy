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
echo "✅ Found happy version: $VERSION"

# Ensure that we depend on the proper version of happy-lib
if grep -Eq "(^| )happy-lib[[:space:]]*==[[:space:]]*$VERSION" "$CABAL_FILE"; then
  echo "✅ happy-lib dependency is correctly listed with version $VERSION in $CABAL_FILE."
else
  echo "❌ happy-lib dependency with version $VERSION is NOT listed in $CABAL_FILE!"
  exit 1
fi

if which wget; then
  if [ "${1:-}" = "--publish" ]; then
    URL="https://hackage.haskell.org/package/happy-lib-$VERSION"
  else
    URL="https://hackage.haskell.org/package/happy-lib-$VERSION/candidate"
  fi
  echo "Checking whether happy-lib has been uploaded ..."
  if ! wget --spider -q "$URL"; then
    echo "❌ happy-lib not found at: $URL. Please run 'lib/hackage-upload.sh $1' first."
    exit 1
  fi
  echo "✅ happy-lib uploaded at: $URL"
else
  echo "❓ Could not find wget; no testing for uploaded happy-lib."
  echo "❓ Be sure that 'lib/hackage-upload.sh $1' has been run first."
fi

sdist=$(mktemp -d $TOP/dist.XXXXXX)
trap 'rm -rf "$sdist"' EXIT

cabal sdist --builddir=$sdist
cabal upload $1 $sdist/sdist/*.tar.gz

echo "✅ happy uploaded"
