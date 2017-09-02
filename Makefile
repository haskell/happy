HAPPY = happy 
HAPPY_OPTS = -agc

ALEX = alex
ALEX_OPTS = -g

sdist ::
	@if [ "`git status -s`" != '' ]; then \
		echo Tree is not clean; \
		exit 1; \
	fi
	$(HAPPY) $(HAPPY_OPTS) src/Parser.ly -o src/Parser.hs
	$(HAPPY) $(HAPPY_OPTS) src/AttrGrammarParser.ly -o src/AttrGrammer.hs
	mv src/Parser.ly src/Parser.ly.boot
	mv src/AttrGrammarParser.ly src/AttrGrammarParser.ly.boot
	cabal sdist
	git checkout .
	git clean -f
