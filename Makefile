ENV = .local.env
HC ?= ghc
CABAL ?= cabal

EXECUTABLE = "happy"
PACKAGES = ["happy", "happy-cli", "happy-grammar", "happy-frontend", "happy-tabular", "happy-backend", "happy-test"]
BOOTSTRAPPING = True

sdist-test ::
	rm -f ${ENV}
	$(CABAL) v2-install --lib happy-test --package-env ${ENV}
	$(HC) -package-env ${ENV} -e 'Happy.Test.SDist.sdist_test "${CABAL}" "$(shell pwd)" ${EXECUTABLE} ${PACKAGES} ${BOOTSTRAPPING}'
	rm -f ${ENV}
