# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.28 1999/07/14 19:33:57 panne Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

# Note: might be overridden from cmd-line (see install rule below)
INSTALLING=0

HC = $(WithHappyHc)

HAPPY    = happy

HS_PROG = happy.bin
HS_SRCS = \
Version.hs \
GenUtils.lhs \
Set.lhs \
IntSet.lhs \
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

SRC_HC_OPTS += -cpp -fglasgow-exts $(HappyHcOpts)

Parser_HC_OPTS      = -H8m -Onot
Grammar_HC_OPTS     = -H8m
LALR_HC_OPTS        = -H12m
ProduceCode_HC_OPTS = -H14m
Main_HC_OPTS        = -H10m

# This is not a good idea for a source distribution
# SRC_HAPPY_OPTS += -g

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

SCRIPT_PROG=happy-$(ProjectVersion)
SCRIPT_OBJS=happy.sh
SCRIPT_LINK=happy
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
	$(HC) -fglasgow-exts Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test
	./happy -g Test.ly
	$(HC) -fglasgow-exts Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test
	./happy -a Test.ly
	$(HC) -fglasgow-exts Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test
	./happy -g -c Test.ly
	$(HC) -fglasgow-exts Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test

include $(TOP)/mk/target.mk

# Hack to re-create the in-situ build tree driver script after 
# having installed it.
#
install ::
	@$(RM) $(SCRIPT_PROG)
	@$(MAKE) $(MFLAGS) BIN_DIST=0 $(SCRIPT_PROG)

