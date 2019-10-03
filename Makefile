CABAL = cabal

HAPPY = happy 
HAPPY_OPTS = -agc
HAPPY_VER = `awk '/^version:/ { print $$2 }' happy.cabal`

ALEX = alex
ALEX_OPTS = -g

SDIST_DIR=dist-newstyle/sdist

GEN = src/gen/Parser.hs src/gen/AttrGrammarParser.hs

all : $(GEN)

src/gen/%.hs : src/boot/%.ly
	$(HAPPY) $(HAPPYFLAGS) $< -o $@

sdist ::
	@case "`$(CABAL) --numeric-version`" in \
		2.[2-9].* | [3-9].* ) ;; \
		* ) echo "Error: needs cabal 2.2.0.0 or later (but got : `$(CABAL) --numeric-version`)" ; exit 1 ;; \
	esac
	@if [ "`git status -s`" != '' ]; then \
		echo Tree is not clean; \
		exit 1; \
	fi
	$(HAPPY) $(HAPPY_OPTS) src/Parser.ly -o src/Parser.hs
	$(HAPPY) $(HAPPY_OPTS) src/AttrGrammarParser.ly -o src/AttrGrammarParser.hs
	mv src/Parser.ly src/Parser.ly.boot
	mv src/AttrGrammarParser.ly src/AttrGrammarParser.ly.boot
	$(CABAL) v2-run gen-happy-sdist 
	cabal v2-sdist
	@if [ ! -f "${SDIST_DIR}/happy-$(HAPPY_VER).tar.gz" ]; then \
		echo "Error: source tarball not found: dist/happy-$(HAPPY_VER).tar.gz"; \
		exit 1; \
	fi
	git checkout .
	git clean -f

sdist-test :: sdist sdist-test-only
	@rm -rf "${SDIST_DIR}/happy-${HAPPY_VER}/"

sdist-test-only ::
	@if [ ! -f "${SDIST_DIR}/happy-$(HAPPY_VER).tar.gz" ]; then \
		echo "Error: source tarball not found: ${SDIST_DIR}/happy-$(HAPPY_VER).tar.gz"; \
		exit 1; \
	fi
	rm -rf "${SDIST_DIR}/happy-$(HAPPY_VER)/"
	tar -xf "${SDIST_DIR}/happy-$(HAPPY_VER).tar.gz" -C ${SDIST_DIR}/
	echo "packages: ." > "${SDIST_DIR}/happy-$(HAPPY_VER)/cabal.project"
	cd "${SDIST_DIR}/happy-$(HAPPY_VER)/" && cabal v2-test --enable-tests all
	@echo ""
	@echo "Success! ${SDIST_DIR}/happy-$(HAPPY_VER).tar.gz is ready for distribution!"
	@echo ""
