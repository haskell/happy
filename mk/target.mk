#-----------------------------------------------------------------------------
# $Id: target.mk,v 1.1 1997/03/28 15:02:31 simonm Exp $
# target.mk project stub
#

# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:
HAPPY_TOP := $(TOP)
TOP:=$(TOP)/..

include $(TOP)/mk/target.mk

# Reset TOP
TOP:=$(HAPPY_TOP)
