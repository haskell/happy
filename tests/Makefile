# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.9 2005/01/14 14:47:23 simonmar Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

TESTS = Test.ly TestMulti.ly TestPrecedence.ly bug001.ly \
	monad001.y monad002.ly precedence001.ly precedence002.y \
	bogus-token.y bug002.y Partial.ly

TEST_HAPPY_OPTS = --strict 

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
HS_PROG_EXT = .exe
else
HS_PROG_EXT = .bin
endif


%.n.hs : %.ly
	$(HAPPY_INPLACE) $(TEST_HAPPY_OPTS) $< -o $@

%.a.hs : %.ly
	$(HAPPY_INPLACE) $(TEST_HAPPY_OPTS) -a $< -o $@

%.g.hs : %.ly
	$(HAPPY_INPLACE) $(TEST_HAPPY_OPTS) -g $< -o $@

%.gc.hs : %.ly
	$(HAPPY_INPLACE) $(TEST_HAPPY_OPTS) -gc $< -o $@

%.ag.hs : %.ly
	$(HAPPY_INPLACE) $(TEST_HAPPY_OPTS) -ag $< -o $@

%.agc.hs : %.ly
	$(HAPPY_INPLACE) $(TEST_HAPPY_OPTS) -agc $< -o $@

%.n.hs : %.y
	$(HAPPY_INPLACE) $(TEST_HAPPY_OPTS) $< -o $@

%.a.hs : %.y
	$(HAPPY_INPLACE) $(TEST_HAPPY_OPTS) -a $< -o $@

%.g.hs : %.y
	$(HAPPY_INPLACE) $(TEST_HAPPY_OPTS) -g $< -o $@

%.gc.hs : %.y
	$(HAPPY_INPLACE) $(TEST_HAPPY_OPTS) -gc $< -o $@

%.ag.hs : %.y
	$(HAPPY_INPLACE) $(TEST_HAPPY_OPTS) -ag $< -o $@

%.agc.hs : %.y
	$(HAPPY_INPLACE) $(TEST_HAPPY_OPTS) -agc $< -o $@

CLEAN_FILES += *.n.hs *.a.hs *.g.hs *.gc.hs *.ag.hs *.agc.hs *.info *.hi *.bin *.exe

ALL_TEST_HS = $(shell echo $(TESTS) | sed -e 's/\([^\. ]*\)\.\(l\)\{0,1\}y/\1.n.hs \1.a.hs \1.g.hs \1.gc.hs \1.ag.hs \1.agc.hs/g')

ALL_TESTS = $(patsubst %.hs, %.run, $(ALL_TEST_HS))

HC_OPTS += -fglasgow-exts -package lang

.PRECIOUS: *.hs *.o *.bin *.exe

%.run : %$(HS_PROG_EXT)
	./$<

%$(HS_PROG_EXT) : %.o
	$(HC) $(HC_OPTS) $($*_LD_OPTS) $< -o $@

all :: $(ALL_TESTS)

check-todo::
	$(HAPPY_INPLACE) -ad Test.ly
	$(HC) Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test
	$(HAPPY_INPLACE) -agd Test.ly
	$(HC) Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test
	$(HAPPY_INPLACE) -agcd Test.ly
	$(HC) Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test

include $(TOP)/mk/target.mk
