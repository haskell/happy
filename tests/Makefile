HAPPY=../dist/build/happy/happy
HC_OPTS=-hide-all-packages -package base -package array -package mtl -Wall -Werror

ifeq ($(wildcard ../cabal.sandbox.config),)
    HC = ghc
    SANDBOX_CONFIG =
else
    HC = cabal exec -- ghc
    SANDBOX_CONFIG = cabal.sandbox.config
endif

.PRECIOUS: %.n.hs %.g.hs %.o %.exe %.bin

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
HS_PROG_EXT = .exe
else
HS_PROG_EXT = .bin
endif

TESTS = Test.ly TestMulti.ly TestPrecedence.ly bug001.ly \
	monad001.y monad002.ly precedence001.ly precedence002.y \
	bogus-token.y bug002.y Partial.ly \
	AttrGrammar001.y AttrGrammar002.y \
	test_rules.y monaderror.y monaderror-explist.y \
	typeclass_monad001.y typeclass_monad002.ly typeclass_monad_lexer.y

ERROR_TESTS = error001.y

TEST_HAPPY_OPTS = --strict --template=..

%.n.hs : %.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) $< -o $@

%.a.hs : %.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) -a $< -o $@

%.g.hs : %.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) -g $< -o $@

%.gc.hs : %.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) -gc $< -o $@

%.ag.hs : %.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) -ag $< -o $@

%.agc.hs : %.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) -agc $< -o $@

%.n.hs : %.y
	$(HAPPY) $(TEST_HAPPY_OPTS) $< -o $@

%.a.hs : %.y
	$(HAPPY) $(TEST_HAPPY_OPTS) -a $< -o $@

%.g.hs : %.y
	$(HAPPY) $(TEST_HAPPY_OPTS) -g $< -o $@

%.gc.hs : %.y
	$(HAPPY) $(TEST_HAPPY_OPTS) -gc $< -o $@

%.ag.hs : %.y
	$(HAPPY) $(TEST_HAPPY_OPTS) -ag $< -o $@

%.agc.hs : %.y
	$(HAPPY) $(TEST_HAPPY_OPTS) -agc $< -o $@

CLEAN_FILES += *.n.hs *.a.hs *.g.hs *.gc.hs *.ag.hs *.agc.hs *.info *.hi *.bin *.exe *.o *.run.stdout *.run.stderr $(SANDBOX_CONFIG)

ALL_TEST_HS = $(shell echo $(TESTS) | sed -e 's/\([^\. ]*\)\.\(l\)\{0,1\}y/\1.n.hs \1.a.hs \1.g.hs \1.gc.hs \1.ag.hs \1.agc.hs/g')

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
	@diff -u --ignore-all-space $*.stdout $*.run.stdout
	@diff -u --ignore-all-space $*.stderr $*.run.stderr

%$(HS_PROG_EXT) : %.hs
	$(HC) $(HC_OPTS) $($*_LD_OPTS) $< -o $@

all :: $(SANDBOX_CONFIG) $(CHECK_ERROR_TESTS) $(ALL_TESTS)

check-todo::
	$(HAPPY) $(TEST_HAPPY_OPTS) -ad Test.ly
	$(HC) Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test
	$(HAPPY) $(TEST_HAPPY_OPTS) -agd Test.ly
	$(HC) Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test
	$(HAPPY) $(TEST_HAPPY_OPTS) -agcd Test.ly
	$(HC) Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test

.PHONY: clean

clean:
	$(RM) $(CLEAN_FILES)

cabal.sandbox.config:
	cabal sandbox init --sandbox=../.cabal-sandbox
