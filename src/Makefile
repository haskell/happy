# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.54 2005/01/18 11:17:42 simonmar Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

INSTALLING=1

HC = $(WithHappyHc)

HAPPY    = happy

ifeq "$(Windows)" "YES"
HS_PROG_EXT = .exe
else
HS_PROG_EXT = .bin
endif

HS_PROG = happy$(HS_PROG_EXT)

EXCLUDED_SRCS = TestLexer.lhs IntSet.lhs

SRC_HC_OPTS += -cpp -fglasgow-exts $(HappyHcOpts)

ghc_lt_503 = $(shell if (test $(GhcCanonVersion) -lt 503); then echo YES; else echo NO; fi)
ifeq "$(ghc_lt_503)" "YES"
SRC_HC_OPTS += -package lang
endif

Parser_HC_OPTS      = -Onot

ifeq "$(Windows)" "YES"
Main_HC_OPTS += -Dmingw32_HOST_OS=1
endif

# GHC opts are not a good idea for a source distribution
GHC_HAPPY_OPTS      =

boot ::
	if [ -d $(FPTOOLS_TOP)/ghc ]; then \
		(cd $(FPTOOLS_TOP)/ghc/utils/unlit && $(MAKE) boot); \
		(cd $(FPTOOLS_TOP)/ghc/includes && $(MAKE) boot); \
		(cd $(FPTOOLS_TOP)/ghc/driver && $(MAKE) boot); \
	fi 


# -----------------------------------------------------------------------------=
# Create the Version.hs file

VERSION_HS = Version.hs
EXTRA_SRCS += $(VERSION_HS)

boot :: $(VERSION_HS)

Version.hs : $(TOP)/mk/version.mk
	@$(RM) -f $(VERSION_HS)
	@echo "Creating $(VERSION_HS) ... "
	@echo "module Version where" >>$(VERSION_HS)
	@echo "version = \"$(ProjectVersion)\"" >> $(VERSION_HS)

# -----------------------------------------------------------------------------

ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)"   "1"
HAPPYLIB=$$\"\"libdir
HAPPYBIN=$$\"\"libexecdir/$(HS_PROG)
else
HAPPYLIB=$(libdir)
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
ifneq "$(Windows)" "YES"
LINK	 	=  happy
endif
else
TOP_PWD 	:= $(FPTOOLS_TOP_ABS)
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

SCRIPT_OBJS=happy.sh

INTERP=$(SHELL)

SCRIPT_SUBST_VARS = 	\
	HAPPYLIB	\
	HAPPYBIN

# The script isn't installed on Windows; we run the binary directly
ifeq "$(Windows)" "NO"
INSTALL_SCRIPTS += $(SCRIPT_PROG)
endif

INSTALL_LIBEXECS = $(HS_PROG)

# Hack to avoid automatic cleaning machinery from cleaning Parser.hs
#
DERIVED_HAPPY_SRCS = 

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean distclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

include $(TOP)/mk/target.mk
