# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.5 1999/10/08 12:00:37 simonmar Exp $

TOP = ../..
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

GENERIC_TEMPLATE = GenericTemplate.hs

INSTALL_DATAS = $(TEMPLATES)

all :: $(TEMPLATES)

override datadir = $(libdir)/happy

HappyTemplate : $(GENERIC_TEMPLATE)
	$(HC) -E -cpp $(GENERIC_TEMPLATE) > $@

HappyTemplate-ghc : $(GENERIC_TEMPLATE)
	$(HC) -E -cpp -DHAPPY_GHC $(GENERIC_TEMPLATE) > $@

HappyTemplate-coerce : $(GENERIC_TEMPLATE)
	$(HC) -E -cpp -DHAPPY_GHC -DHAPPY_COERCE $(GENERIC_TEMPLATE) > $@

HappyTemplate-arrays : $(GENERIC_TEMPLATE)
	$(HC) -E -cpp -DHAPPY_ARRAY $(GENERIC_TEMPLATE) > $@

HappyTemplate-arrays-ghc : $(GENERIC_TEMPLATE)
	$(HC) -E -cpp -DHAPPY_GHC -DHAPPY_ARRAY $(GENERIC_TEMPLATE) > $@

HappyTemplate-arrays-coerce : $(GENERIC_TEMPLATE)
	$(HC) -E -cpp -DHAPPY_GHC -DHAPPY_ARRAY -DHAPPY_COERCE $(GENERIC_TEMPLATE) > $@

HappyTemplate-arrays-debug : $(GENERIC_TEMPLATE)
	$(HC) -E -cpp -DHAPPY_ARRAY -DHAPPY_DEBUG $(GENERIC_TEMPLATE) > $@

HappyTemplate-arrays-ghc-debug : $(GENERIC_TEMPLATE)
	$(HC) -E -cpp -DHAPPY_GHC -DHAPPY_ARRAY -DHAPPY_DEBUG $(GENERIC_TEMPLATE) > $@

HappyTemplate-arrays-coerce-debug : $(GENERIC_TEMPLATE)
	$(HC) -E -cpp -DHAPPY_GHC -DHAPPY_ARRAY -DHAPPY_COERCE -DHAPPY_DEBUG $(GENERIC_TEMPLATE) > $@

CLEAN_FILES += $(TEMPLATES)

include $(TOP)/mk/target.mk
