CABAL = cabal

HAPPY_VER = `awk '/^version:/ { print $$2 }' happy.cabal`

ALEX = alex
ALEX_OPTS = -g

SDIST_DIR=dist-newstyle/sdist

sdist ::
	@case "`$(CABAL) --numeric-version`" in \
		2.[2-9].* | [3-9].* ) ;; \
		* ) echo "Error: needs cabal 2.2.0.0 or later (but got : `$(CABAL) --numeric-version`)" ; exit 1 ;; \
	esac
	@if [ "`git status -s`" != '' ]; then \
		echo Tree is not clean; \
		exit 1; \
	fi
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
