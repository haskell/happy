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
	issue91.y issue93.y issue94.y issue95.y \
	AttrGrammar001.y AttrGrammar002.y \
	test_rules.y monaderror.y monaderror-explist.y \
	typeclass_monad001.y typeclass_monad002.ly typeclass_monad_lexer.y

ERROR_TESTS = error001.y

TEST_HAPPY_OPTS = --strict --template=..

%.p.ly : %.ly
	gcc -x c -P -E $< -o $@

%.p.y : %.y
	gcc -x c -P -E $< -o $@

%.hp.ly : %.ly
	gcc -x c -P -E $< -o $@ -DQUALIFIEDPRELUDE=HappyPrelude

%.hp.y : %.y
	gcc -x c -P -E $< -o $@ -DQUALIFIEDPRELUDE=HappyPrelude

%.n.hs : %.p.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) $< -o $@

%.hp.n.hs : %.hp.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) --noprelude $< -o $@

%.a.hs : %.p.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) -a $< -o $@

%.hp.a.hs : %.hp.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) --noprelude -a $< -o $@

%.g.hs : %.p.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) -g $< -o $@

%.hp.g.hs : %.hp.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) --noprelude -g $< -o $@

%.gc.hs : %.p.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) -gc $< -o $@

%.hp.gc.hs : %.hp.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) --noprelude -gc $< -o $@

%.ag.hs : %.p.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) -ag $< -o $@

%.hp.ag.hs : %.hp.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) --noprelude -ag $< -o $@

%.agc.hs : %.p.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) -agc $< -o $@

%.hp.agc.hs : %.hp.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) --noprelude -agc $< -o $@

%.n.hs : %.p.y
	$(HAPPY) $(TEST_HAPPY_OPTS) $< -o $@

%.hp.n.hs : %.hp.y
	$(HAPPY) $(TEST_HAPPY_OPTS) --noprelude $< -o $@

%.a.hs : %.p.y
	$(HAPPY) $(TEST_HAPPY_OPTS) -a $< -o $@

%.hp.a.hs : %.hp.y
	$(HAPPY) $(TEST_HAPPY_OPTS) --noprelude -a $< -o $@

%.g.hs : %.p.y
	$(HAPPY) $(TEST_HAPPY_OPTS) -g $< -o $@

%.hp.g.hs : %.hp.y
	$(HAPPY) $(TEST_HAPPY_OPTS) --noprelude -g $< -o $@

%.gc.hs : %.p.y
	$(HAPPY) $(TEST_HAPPY_OPTS) -gc $< -o $@

%.hp.gc.hs : %.hp.y
	$(HAPPY) $(TEST_HAPPY_OPTS) --noprelude -gc $< -o $@

%.ag.hs : %.p.y
	$(HAPPY) $(TEST_HAPPY_OPTS) -ag $< -o $@

%.hp.ag.hs : %.hp.y
	$(HAPPY) $(TEST_HAPPY_OPTS) --noprelude -ag $< -o $@

%.agc.hs : %.p.y
	$(HAPPY) $(TEST_HAPPY_OPTS) -agc $< -o $@

%.hp.agc.hs : %.hp.y
	$(HAPPY) $(TEST_HAPPY_OPTS) --noprelude -agc $< -o $@

CLEAN_FILES += *.p.ly *.p.y *.hp.ly *.hp.y *.n.hs *.a.hs *.g.hs *.gc.hs *.ag.hs *.agc.hs *.info *.hi *.bin *.exe *.o *.run.stdout *.run.stderr $(SANDBOX_CONFIG)

PREPROCESS_TESTS = $(shell echo $(TESTS) | sed -e 's/\([^\. ]*\)\.\(l\)\{0,1\}y/\1.p.\2y \1.hp.\2y/g')

ALL_TEST_HS = $(shell echo $(TESTS) | sed -e 's/\([^\. ]*\)\.\(l\)\{0,1\}y/\1.n.hs \1.hp.n.hs \1.a.hs \1.hp.a.hs \1.g.hs \1.hp.g.hs \1.gc.hs \1.hp.gc.hs \1.ag.hs \1.hp.ag.hs \1.agc.hs \1.hp.agc.hs/g')

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

all :: $(SANDBOX_CONFIG) $(PREPROCESS_TESTS) $(CHECK_ERROR_TESTS) $(ALL_TESTS)

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
