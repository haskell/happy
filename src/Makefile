# $Id: Makefile,v 1.1 1997/02/11 13:19:35 simonm Exp $

#-----------------------------------------------------------------------------
# How to install happy using a compiler.
#
# (1) Decide which compiler to use (ghc or hbc) and set up the HC
#     variable below.
#
# (2) Decide where the HappyTemplate files will go, and put the
#     directory name in the HAPPYLIB variable.
#
# (3) set up the correct COMPILER_SPEC_FLAGS and OTHER_FLAGS
#     definitions for your compiler.
#  
# (4) Decide where the binary will go, and set the BINDIR variable.
#
# (5) Type make.
#
# (6) to test happy, type make test
#
# (7) If everything seems ok, try make install
#
# (8) Install the info manual in your info directory and/or print out
#     the postscript documentation.
#
#
# NOTE: The file Parser.ly needs this new version of Happy (0.8),
# so be careful not to delete Parser.hs.
#
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# How to use Happy with Gofer (2.30)
#
# (1) Type `make happy.prj'
#
# (2) Type `export GOFER=haskell.prelude' (if you're using an sh derivative)
#     or `setenv GOFER haskell.prelude' (if you're using a csh derivative)
#
# (3) Type `gofer + happy.prj'
#
# (4) To get Happy to generate a parser, you'll have to hack in the
#     command line arguments by hand in the file Main.lhs, function
#     `constArgs'.  Yes it is a pain, so get a compiled binary
#     instead.
#
# (5) If gofer bombs out with an `out of type variables in type
#     checker' type message, recompile gofer with more type variables.
#    
# We strongly recommend that you compile happy with a Haskell compiler
# to produce a happy binary, even if the produced parsers are ultimately
# compiled under gofer. The gofer compability is only included because
# we found it useful when developing Happy.
#
#-----------------------------------------------------------------------------

.SUFFIXES: .o .lhs .hi .hs .ly
RM = rm -f
VERSION = 1.1-simonm

BINDIR   = /users/ets/simonm/bin/sparc-sun-sunos4
HAPPYLIB = /users/ets/simonm/src/haskell/happy/templates

# In the next three definitions, the first one is for ghc, and the
# second is for hbc, and the third for nhc

HAPPY			= happy -g
HC 			= ghc-0.26
#HC 			= hbc
#HC			= nhc

COMPILER_SPEC_FLAGS 	=  -cpp -fhaskell-1.3 -fglasgow-exts -H12M -hi-diffs 
#COMPILER_SPEC_FLAGS 	=  -O -fpbu -M -H15M
#COMPILER_SPEC_FLAGS 	=  -O -cpp -v

OPT 			=

HCFLAGS			= $(OPT) $(COMPILER_SPEC_FLAGS)


SRCS = \
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

PROG=happy
GOFER_PROJECT = happy.prj

project: $(PROG)

Version.hs: Makefile
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

install: happy
	install -s -m 755 happy $(BINDIR)/happy
	if [ ! -d $(HAPPYLIB) ]; then \
		mkdir -p $(HAPPYLIB); \
	fi; \
	cd ../templates; \
	for i in *; do \
		cp $$i $(HAPPYLIB)/$$i; \
	done


package:
	gtar czvf happy.tar.gz \
		$(SRCS) \
		Parser.ly \
		Makefile \
		README \
		happy.prj haskell.prelude \
		doc/happy.info \
		doc/happy.texinfo \
		doc/happy.ps \
		doc/happy.dvi \
		doc/Makefile \
		Test.ly Test.hs-CMP \
		templates/HappyTemplate \
		templates/HappyTemplate-arrays \
		templates/HappyTemplate-ghc \
		examples

test:
	./happy - --template templates Test.ly
	-diff Test.hs Test.hs-CMP
	$(HC) -O -H20M Test.hs -o happy_test
	./happy_test
	-rm -f ./happy_test

#-----------------------------------------------------------------------------
# Generic make rules for haskell programs.
# Define the following variables: SRCS, PROG, GOFER_PROJECT.

.ly.hs:
	$(RM) $@
	$(HAPPY) $(HAPPYFLAGS) $<

.lhs.o:
	$(RM) $@
	$(HC) -c $(HCFLAGS) $< -o $*.o

.hs.o :
	$(RM) $@
	$(HC) -c $(HCFLAGS) $< -o $*.o

.o.hi:
	@if [ ! -f $@ ] ; then \
	echo $(RM) $< ; \
	$(RM) $< ; \
	set +e ; \
	echo $(MAKE) $< ; \
	$(MAKE) $< ; \
	if [ $$? -ne 0 ] ; then \
	exit 1; \
	fi ; \
	fi

$(GOFER_PROJECT): Makefile
	echo "-- Project Files" > $(GOFER_PROJECT)
	echo "-- This list is automatically generated, so edit the Makefile" \
				>> $(GOFER_PROJECT) 
	echo $(SRCS) | tr ' ' '\012' >> $(GOFER_PROJECT)

OFILES1 = $(SRCS:.lhs=.o)
OFILES = $(OFILES1:.hs=.o)

$(PROG): $(OFILES)
	$(HC) -o $(PROG) $(HCFLAGS) $(HCFILES) $(OFILES)

graph:
	egrep -v '!' $(PROG).hp > $(PROG)2.hp
	hp2ps $(PROG)2.hp
	ghostview $(PROG)2.ps

ngraph:
	hp2ps $(PROG).hp
	ghostview $(PROG).ps

clean:
	$(RM) *.o *~ *.bak

cleaner: clean
	$(RM) *.hi

depend:
	mkdependHS $(SRCS)
	chmod u+w Makefile

# DO NOT DELETE: Beginning of Haskell dependencies
Version.o : Version.hs
GenUtils.o : GenUtils.lhs
Set.o : Set.lhs
Set.o : ./GenUtils.hi
ParseMonad.o : ParseMonad.lhs
Lexer.o : Lexer.lhs
Lexer.o : ./ParseMonad.hi
Lexer.o : ./GenUtils.hi
AbsSyn.o : AbsSyn.lhs
AbsSyn.o : ./GenUtils.hi
Grammar.o : Grammar.lhs
Grammar.o : ./GenUtils.hi
Grammar.o : ./AbsSyn.hi
Parser.o : Parser.hs
Parser.o : ./GenUtils.hi
Parser.o : ./AbsSyn.hi
Parser.o : ./Lexer.hi
First.o : First.lhs
First.o : ./GenUtils.hi
First.o : ./Set.hi
First.o : ./AbsSyn.hi
First.o : ./Grammar.hi
LALR.o : LALR.lhs
LALR.o : ./GenUtils.hi
LALR.o : ./Set.hi
LALR.o : ./AbsSyn.hi
LALR.o : ./Grammar.hi
LALR.o : ./First.hi
Target.o : Target.lhs
ProduceCode.o : ProduceCode.lhs
ProduceCode.o : ./Version.hi
ProduceCode.o : ./GenUtils.hi
ProduceCode.o : ./AbsSyn.hi
ProduceCode.o : ./Grammar.hi
ProduceCode.o : ./Target.hi
Info.o : Info.lhs
Info.o : ./Version.hi
Info.o : ./LALR.hi
Info.o : ./GenUtils.hi
Info.o : ./Set.hi
Info.o : ./AbsSyn.hi
Info.o : ./Grammar.hi
Info.o : ./ProduceCode.hi
GetOpt.o : GetOpt.lhs
GetOpt.o : ./GenUtils.hi
Main.o : Main.lhs
Main.o : ./GenUtils.hi
Main.o : ./Lexer.hi
Main.o : ./AbsSyn.hi
Main.o : ./Grammar.hi
Main.o : ./Parser.hi
Main.o : ./First.hi
Main.o : ./LALR.hi
Main.o : ./Version.hi
Main.o : ./ProduceCode.hi
Main.o : ./Info.hi
Main.o : ./Target.hi
Main.o : ./GetOpt.hi
# DO NOT DELETE: End of Haskell dependencies
