# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2001/04/27 11:10:14 simonmar Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

TESTS = Test.ly TestMulti.ly TestPrecedence.ly bug001.ly monad001.y monad002.ly

%.n.hs : %.ly
	$(HAPPY_INPLACE) $< -o $@

%.a.hs : %.ly
	$(HAPPY_INPLACE) -a $< -o $@

%.g.hs : %.ly
	$(HAPPY_INPLACE) -g $< -o $@

%.gc.hs : %.ly
	$(HAPPY_INPLACE) -gc $< -o $@

%.ag.hs : %.ly
	$(HAPPY_INPLACE) -ag $< -o $@

%.agc.hs : %.ly
	$(HAPPY_INPLACE) -agc $< -o $@

%.n.hs : %.y
	$(HAPPY_INPLACE) $< -o $@

%.a.hs : %.y
	$(HAPPY_INPLACE) -a $< -o $@

%.g.hs : %.y
	$(HAPPY_INPLACE) -g $< -o $@

%.gc.hs : %.y
	$(HAPPY_INPLACE) -gc $< -o $@

%.ag.hs : %.y
	$(HAPPY_INPLACE) -ag $< -o $@

%.agc.hs : %.y
	$(HAPPY_INPLACE) -agc $< -o $@

CLEAN_FILES += *.n.hs *.a.hs *.g.hs *.gc.hs *.ag.hs *.agc.hs *.info *.hi

ALL_TEST_HS = $(shell echo $(TESTS) | sed -e 's/\([^\. ]*\)\.\(l\)\{0,1\}y/\1.n.hs \1.a.hs \1.g.hs \1.gc.hs \1.ag.hs \1.agc.hs/g')

ALL_TESTS = $(patsubst %.hs, %.run, $(ALL_TEST_HS))

HC_OPTS += -fglasgow-exts -package lang

%.run : %.bin
	./$<

%.bin : %.o
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
