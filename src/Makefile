# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.8 1997/09/03 23:27:38 sof Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

HAPPY    = happy-new

# must compile with GHC 2.xx
ifeq ($(Ghc2_0),NO)
HC       = ../../ghc/driver/ghc
endif

MKDEPENDHS      := $(HC)

VERSION  = 1.3
HAPPYLIB = $(FPTOOLS_TOP_ABS)/happy/templates

# Todo: install templates properly, building new Version.hs when
# 'make install' is requested.

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

boot :: Version.hs

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
