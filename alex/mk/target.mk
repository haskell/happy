#-----------------------------------------------------------------------------
# $Id: target.mk,v 1.1 2002/12/09 21:51:51 ijones Exp $
# target.mk project stub
#

# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:
ALEX_TOP := $(TOP)
TOP:=$(TOP)/..

include $(TOP)/mk/target.mk

ALEX_INPLACE = $(ALEX_TOP)/src/alex-inplace

# Reset TOP
TOP:=$(ALEX_TOP)
