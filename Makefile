ENV = .local.env

EXECUTABLE = "happy"
PACKAGES = ["happy", "happy-core", "happy-frontend", "happy-middleend", "happy-backend", "happy-test"]
BOOTSTRAPPING = True

sdist-test ::
	rm -f ${ENV}
	cabal v2-install --lib happy-test --package-env ${ENV}
	ghc -package-env ${ENV} -e 'SDist.sdist_test "$(shell pwd)" ${EXECUTABLE} ${PACKAGES} ${BOOTSTRAPPING}'
	rm -f ${ENV}