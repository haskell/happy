# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.41 2001/09/24 14:12:26 simonmar Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

INSTALLING=1

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

SRC_HC_OPTS += -cpp -fglasgow-exts -package lang $(HappyHcOpts)

Parser_HC_OPTS      = -Onot
Grammar_HC_OPTS     = -H8m
LALR_HC_OPTS        = -H12m
ProduceCode_HC_OPTS = -H16m
Main_HC_OPTS        = -H10m
Version_HC_OPTS	    = -DHAPPY_VERSION=$(ProjectVersion)

# This is not a good idea for a source distribution
# SRC_HAPPY_OPTS += -g

boot ::
	if [ -d $(FPTOOLS_TOP)/ghc ]; then \
		(cd $(FPTOOLS_TOP)/ghc/utils/unlit && $(MAKE) boot); \
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

INSTALLED_SCRIPT_PROG  = happy-$(ProjectVersion)
INPLACE_SCRIPT_PROG    = happy-inplace

ifeq "$(INSTALLING)" "1"
TOP_PWD 	:= $(prefix)
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
LINK	 	=  happy
else
TOP_PWD 	:= $(FPTOOLS_TOP_ABS)
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

SCRIPT_OBJS=happy.sh

INTERP=$(SHELL)

SCRIPT_SUBST_VARS = 	\
	HAPPYLIB	\
	HAPPYBIN

INSTALL_SCRIPTS += $(SCRIPT_PROG)
INSTALL_LIBEXECS = $(HS_PROG)

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean distclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

include $(TOP)/mk/target.mk
