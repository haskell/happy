# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.1.2.2 2003/02/18 17:13:49 simonmar Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

TEMPLATES = \
	AlexTemplate \
	AlexTemplate-ghc \
	AlexTemplate-ghc-debug \
	AlexTemplate-debug \
	AlexTemplate-monad \
	AlexTemplate-monad-ghc \
	AlexTemplate-monad-ghc-debug \
	AlexTemplate-monad-debug

GENERIC_TEMPLATE = GenericTemplate.hs

INSTALL_DATAS = $(TEMPLATES)

all :: $(TEMPLATES)

override datadir = $(libdir)

ghc_411_at_least = $(shell expr "$(GhcMajVersion)"  =   4 \& \
                                "$(GhcMinVersion)" \>= 11 \| \
                                "$(GhcMajVersion)" \>   4)

ifeq "$(ghc_411_at_least)" "1"
CPP_IT = $(GHC) -E -cpp -o
else
CPP_IT = $(GHC) -E -cpp >
endif

AlexTemplate.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ $(GENERIC_TEMPLATE)

AlexTemplate-ghc.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DALEX_GHC $(GENERIC_TEMPLATE)

AlexTemplate-ghc-debug.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DALEX_GHC -DALEX_DEBUG $(GENERIC_TEMPLATE)

AlexTemplate-debug.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DALEX_DEBUG $(GENERIC_TEMPLATE)

AlexTemplate-monad.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DALEX_MONAD $(GENERIC_TEMPLATE)

AlexTemplate-monad-ghc.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DALEX_MONAD -DALEX_GHC $(GENERIC_TEMPLATE)

AlexTemplate-monad-ghc-debug.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DALEX_MONAD -DALEX_GHC -DALEX_DEBUG $(GENERIC_TEMPLATE)

AlexTemplate-monad-debug.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DALEX_MONAD -DALEX_DEBUG $(GENERIC_TEMPLATE)

# hack to turn cpp-style '# 27 "GenericTemplate.hs"' into 
# '{-# LINE 27 "GenericTemplate.hs" #-}'.
% : %.hspp
	perl -pe 's/^#\s+(\d+)\s+(\"[^\"]*\")/{-# LINE \1 \2 #-}/g;s/\$$(Id:.*)\$$/\1/g' < $< > $@

CLEAN_FILES += $(TEMPLATES) $(patsubst %, %.hspp, $(TEMPLATES))

include $(TOP)/mk/target.mk
