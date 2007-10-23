# Put the Happy-generated .hs files in the right place in the source dist.
set -e
rm -f dist/happy-*.tar.gz
rm -rf dist/happy-*/
./Setup sdist
cd dist
tar xvzf happy-*.tar.gz
cd happy-*/
mkdir dist
mkdir dist/build
mv Parser.hs AttrGrammarParser.hs dist/build
cd ..
tar cvzf happy-*.tar.gz happy-*/
