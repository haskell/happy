# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.17 1997/09/24 10:17:11 simonm Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

HC = $(WithHappyHc)

HAPPY    = happy -1.2

MKDEPENDHS      := $(FPTOOLS_TOP)/ghc/driver/ghc

VERSION  = 1.3

HS_PROG = happy
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

boot :: Version.hs

boot ::
	if [ -d $(FPTOOLS_TOP)/ghc ]; then \
		(cd $(FPTOOLS_TOP)/ghc/utils/unlit && $(MAKE) boot); \
		(cd $(FPTOOLS_TOP)/ghc/utils/mkdependHS && $(MAKE) boot); \
		(cd $(FPTOOLS_TOP)/ghc/driver && $(MAKE) boot); \
	fi 

ifeq "$(INSTALLING)" "1"
HAPPYLIB = $(libdir)/happy
else
HAPPYLIB = $(FPTOOLS_TOP_ABS)/happy/templates
endif

INSTALL_PROGS = happy

install ::
	@$(RM) Version.hs Version.o
	@$(MAKE) $(MFLAGS) INSTALLING=1 $(HS_PROG)

Version.hs : Makefile
	@-rm -f Version.hs-NEW
	@echo "module Version where"			>  Version.hs-NEW
	@echo "version = \""$(VERSION)"\""		>> Version.hs-NEW
	@echo "template_dir = \""$(HAPPYLIB)"\""	>> Version.hs-NEW
	@if [ ! -f Version.hs ]; then \
		echo Making Version.hs version=$(VERSION) template_dir=$(HAPPYLIB); \
		cp Version.hs-NEW Version.hs ; \
	elif diff Version.hs Version.hs-NEW ; \
		then rm Version.hs-NEW ;\
		else \
		echo Remaking Version.hs version=$(VERSION) template_dir=$(HAPPYLIB); \
		rm -f Version.hs ; \
		mv Version.hs-NEW Version.hs; \
	fi

check::
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
