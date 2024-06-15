# NOTE: `cabal test` will take care to build the local `happy`
# executable and place it into $PATH for us to pick up.
#
# If it doesn't look like the alex binary in $PATH comes from the
# build tree, then we'll fall back to pointing to
# ../dist/build/alex/alex to support running tests via "runghc
# Setup.hs test".
#
# If HAPPY has been set outside, e.g. in the environment, we trust this setting.
# This way, we can pass in the correct Happy executable from a CI environment
# without danger of it being "fixed" by the logic below.
# [2021-07-14, PR #196](https://github.com/haskell/happy/pull/196)
#
ifndef HAPPY
HAPPY=$(shell which happy)
ifeq "$(filter $(dir $(shell pwd))%,$(HAPPY))" ""
HAPPY=../dist/build/happy/happy
endif
endif

# NOTE: This assumes that a working `ghc` is on $PATH; this may not
# necessarily be the same GHC used by `cabal` for building `happy`.
#
# Again, if HC has been set in the environment (e.g. by the CI), we keep this setting.
# [2021-07-14, PR #196](https://github.com/haskell/happy/pull/196)
#
HC ?= ghc
HC_OPTS=-Wall -Werror

.PRECIOUS: %.n.hs %.o %.exe %.bin

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
HS_PROG_EXT = .exe
else
HS_PROG_EXT = .bin
endif

TESTS = Test.ly TestMulti.ly TestPrecedence.ly bug001.ly \
	monad001.y monad002.ly precedence001.ly precedence002.y \
	bogus-token.y bug002.y Partial.ly \
	issue91.y issue93.y issue94.y issue95.y \
	test_rules.y monaderror.y monaderror-explist.y \
	typeclass_monad001.y typeclass_monad002.ly typeclass_monad_lexer.y \
	rank2.y shift01.y \
	AttrGrammar001.y AttrGrammar002.y

ERROR_TESTS = error001.y

# NOTE: `cabal` will set the `happy_datadir` env-var accordingly before invoking the test-suite
#TEST_HAPPY_OPTS = --strict --template=..
TEST_HAPPY_OPTS = --strict

%.n.hs : %.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) $< -o $@

%.c.hs : %.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) -c $< -o $@

%.n.hs : %.y
	$(HAPPY) $(TEST_HAPPY_OPTS) $< -o $@

%.c.hs : %.y
	$(HAPPY) $(TEST_HAPPY_OPTS) -c $< -o $@

CLEAN_FILES += *.n.hs *.c.hs *.info *.hi *.bin *.exe *.o *.run.stdout *.run.stderr

ALL_TEST_HS = $(shell echo $(TESTS) | sed -e 's/\([^\. ]*\)\.\(l\)\{0,1\}y/\1.n.hs \1.c.hs/g')

ALL_TESTS = $(patsubst %.hs, %.run, $(ALL_TEST_HS))

CHECK_ERROR_TESTS = $(patsubst %, check.%, $(ERROR_TESTS))

HC_OPTS += -fforce-recomp

.PRECIOUS: %.hs %.o %.bin %.$(HS_PROG_EXT)

%.run : %$(HS_PROG_EXT)
	@echo "--> Checking $<..."
	./$<

check.%.y : %.y
	@echo "--> Checking $<..."
	$(HAPPY) $(TEST_HAPPY_OPTS) $< 1>$*.run.stdout 2>$*.run.stderr || true
	sed -i '/^Up to date$$/d' $*.run.stdout $*.run.stderr
	@diff -u --ignore-all-space $*.stdout $*.run.stdout
	@diff -u --ignore-all-space $*.stderr $*.run.stderr

%$(HS_PROG_EXT) : %.hs
	$(HC) $(HC_OPTS) $($*_LD_OPTS) $< -o $@

all :: $(CHECK_ERROR_TESTS) $(ALL_TESTS)

check-todo::
	$(HAPPY) $(TEST_HAPPY_OPTS) -d Test.ly
	$(HC) Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test
	$(HAPPY) $(TEST_HAPPY_OPTS) -cd Test.ly
	$(HC) Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test

.PHONY: clean

clean:
	$(RM) $(CLEAN_FILES)
