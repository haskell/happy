#-----------------------------------------------------------------------------
# $Id: target.mk,v 1.2 2000/07/13 10:23:01 simonmar Exp $
# target.mk project stub
#

# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:
HAPPY_TOP := $(TOP)
TOP:=$(TOP)/..

include $(TOP)/mk/target.mk

HAPPY_INPLACE = $(HAPPY_TOP)/src/happy-inplace

# Reset TOP
TOP:=$(HAPPY_TOP)
