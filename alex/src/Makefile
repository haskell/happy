# -----------------------------------------------------------------------------
# $Id: 

# alex Makefile, Isaac Jones
# The make uninstall rule doesn't work, but neither does Happy's seem to work

TOP = ..
include $(TOP)/mk/boilerplate.mk

INSTALLING=1

ALEX    = alex

HS_PROG = alex.bin

Parser_HC_OPTS      = -Onot
Version_HC_OPTS	    = -DALEX_VERSION=$(ProjectVersion)

# This is not a good idea for a source distribution
# SRC_ALEX_OPTS += -g

boot ::
	if [ -d $(FPTOOLS_TOP)/ghc ]; then \
		(cd $(FPTOOLS_TOP)/ghc/utils/unlit && $(MAKE) boot); \
		(cd $(FPTOOLS_TOP)/ghc/includes && $(MAKE) boot); \
		(cd $(FPTOOLS_TOP)/ghc/driver && $(MAKE) boot); \
	fi 

ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)"   "1"
ALEXLIB=$$\"\"libdir/alex
ALEXBIN=$$\"\"libexecdir/$(HS_PROG)
else
ALEXLIB=$(libdir)/alex
ALEXBIN=$(libexecdir)/$(HS_PROG)
endif # BIN_DIST
else
#ALEXLIB=$(FPTOOLS_TOP_ABS)/alex/templates
ALEXBIN=$(FPTOOLS_TOP_ABS)/alex/src/$(HS_PROG)
endif

ifeq "$(INSTALLING)" "1"
TOP_PWD 	:= $(prefix)
else
TOP_PWD 	:= $(FPTOOLS_TOP_ABS)
endif

#SCRIPT_OBJS=happy.sh

INTERP=$(SHELL)

SCRIPT_SUBST_VARS = 	\
	ALEXLIB	\
	ALEXBIN

INSTALL_EXECS = $(HS_PROG)
INSTALL_PROGS = $(HS_PROG)
INSTALL_LIBS = Alex.hs Alex.o Alex.hi Scan.hs Scan.o Scan.hi

# Hack to avoid automatic cleaning machinery from cleaning Parser.hs
#
DERIVED_ALEX_SRCS = 

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean distclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

include $(TOP)/mk/target.mk
