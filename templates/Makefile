# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.12 2004/08/11 15:39:32 paulcc Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

TEMPLATES = \
	HappyTemplate \
	HappyTemplate-ghc \
	HappyTemplate-coerce \
	HappyTemplate-arrays \
	HappyTemplate-arrays-ghc \
	HappyTemplate-arrays-coerce \
	HappyTemplate-arrays-debug \
	HappyTemplate-arrays-ghc-debug \
	HappyTemplate-arrays-coerce-debug \
	GLR_Base \
	GLR_Lib \
	GLR_Lib-ghc \

GENERIC_TEMPLATE = GenericTemplate.hs
GLR_TEMPLATE = GLR_Lib.lhs

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

HappyTemplate.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ $(GENERIC_TEMPLATE)

HappyTemplate-ghc.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DHAPPY_GHC $(GENERIC_TEMPLATE)

HappyTemplate-coerce.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DHAPPY_GHC -DHAPPY_COERCE $(GENERIC_TEMPLATE)

HappyTemplate-arrays.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DHAPPY_ARRAY $(GENERIC_TEMPLATE)

HappyTemplate-arrays-ghc.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DHAPPY_GHC -DHAPPY_ARRAY $(GENERIC_TEMPLATE)

HappyTemplate-arrays-coerce.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DHAPPY_GHC -DHAPPY_ARRAY -DHAPPY_COERCE $(GENERIC_TEMPLATE)

HappyTemplate-arrays-debug.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DHAPPY_ARRAY -DHAPPY_DEBUG $(GENERIC_TEMPLATE)

HappyTemplate-arrays-ghc-debug.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DHAPPY_GHC -DHAPPY_ARRAY -DHAPPY_DEBUG $(GENERIC_TEMPLATE)

HappyTemplate-arrays-coerce-debug.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DHAPPY_GHC -DHAPPY_ARRAY -DHAPPY_COERCE -DHAPPY_DEBUG $(GENERIC_TEMPLATE)

GLR_Base.hspp : GLR_Base.lhs
	$(CPP_IT) $@ GLR_Base.lhs

GLR_Lib.hspp : $(GLR_TEMPLATE)
	$(CPP_IT) $@ $(GLR_TEMPLATE)

GLR_Lib-ghc.hspp : $(GLR_TEMPLATE)
	$(CPP_IT) $@ -DHAPPY_GHC $(GLR_TEMPLATE)

# hack to turn cpp-style '# 27 "GenericTemplate.hs"' into 
# '{-# LINE 27 "GenericTemplate.hs" #-}'.
% : %.hspp
	perl -pe 's/^#\s+(\d+)\s+(\"[^\"]*\")/{-# LINE \1 \2 #-}/g;s/\$$(Id:.*)\$$/\1/g' < $< > $@

CLEAN_FILES += $(TEMPLATES) $(patsubst %, %.hspp, $(TEMPLATES))

include $(TOP)/mk/target.mk
