# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 1997/03/27 14:22:56 simonm Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

HC=ghc-0.29
HAPPY=happy-new

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

SRC_HC_OPTS += -cpp -fhaskell-1.3 -fglasgow-exts -Rghc-timing -H12m

include $(TOP)/mk/target.mk
