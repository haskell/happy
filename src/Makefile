# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.19 1997/10/13 08:48:49 simonm Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

# Note: might be overridden from cmd-line (see install rule below)
INSTALLING=0

HC = $(WithHappyHc)

HAPPY    = happy -1.2

MKDEPENDHS      := $(FPTOOLS_TOP)/ghc/driver/ghc

VERSION  = 1.3

HS_PROG = happy.bin
HS_SRCS = \
Version.hs \
GenUtils.lhs \
Set.lhs \
ParseMonad.lhs \
Lexer.lhs \
AbsSyn.lhs \
Grammar.lhs \
Parser.hs \
First.lhs \
LALR.lhs \
Target.lhs \
ProduceCode.lhs \
Info.lhs \
GetOpt.lhs \
Main.lhs 

SRC_HC_OPTS += -cpp -fhaskell-1.3 -fglasgow-exts $(HappyHcOpts)

Parser_HC_OPTS = -Onot

boot ::
	if [ -d $(FPTOOLS_TOP)/ghc ]; then \
		(cd $(FPTOOLS_TOP)/ghc/utils/unlit && $(MAKE) boot); \
		(cd $(FPTOOLS_TOP)/ghc/utils/mkdependHS && $(MAKE) boot); \
		(cd $(FPTOOLS_TOP)/ghc/driver && $(MAKE) boot); \
	fi 


ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)"   "1"
HAPPYLIB=$$\"\"libdir/happy
HAPPYBIN=$$\"\"libexecdir/$(HS_PROG)
else
HAPPYLIB=$(libdir)/happy
HAPPYBIN=$(libexecdir)/$(HS_PROG)
endif # BIN_DIST
else
HAPPYLIB=$(FPTOOLS_TOP_ABS)/happy/templates
HAPPYBIN=$(FPTOOLS_TOP_ABS)/happy/src/$(HS_PROG)
endif

SCRIPT_PROG=happy
SCRIPT_OBJS=happy.sh
INTERP=$(SHELL)

SCRIPT_SUBST_VARS = 	\
	HAPPYLIB	\
	HAPPYBIN

INSTALL_SCRIPTS += $(SCRIPT_PROG)
INSTALL_LIBEXECS = $(HS_PROG)

install ::
	@$(RM) $(SCRIPT_PROG)
	@$(MAKE) $(MFLAGS) INSTALLING=1 $(SCRIPT_PROG)

check ::
	./happy Test.ly
	-diff Test.hs Test.hs-CMP
	$(HC) Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test
	./happy -g Test.ly
	-diff Test.hs Test.hs-CMP-g
	$(HC) -fglasgow-exts Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test
	./happy -a Test.ly
	-diff Test.hs Test.hs-CMP-a
	$(HC) Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test

include $(TOP)/mk/target.mk
