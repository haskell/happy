.SUFFIXES: .y .lhs .exe

PROG=bio-eg

GHC=ghc 
# -dshow-passes
HAPPY=happy

# filtering causes this example to fail...
FILTER = --filter
FILTER =

.hi.o :
	@ dummy 

${PROG} : Bio.o Main.lhs DV_lhs
	${GHC} -cpp -fglasgow-exts -package lang -o ${PROG} --make Main.lhs

DV_lhs :
	ln -s ../common/DaVinciTypes.hs ../common/DV_lhs .
                                                                                
BioData.hs Bio.hs : Bio.y
	${HAPPY} --info --glr --ghc ${FILTER} $< 

Bio.o : Bio.hs BioData.hi
	${GHC} -cpp -fglasgow-exts -package lang -O2 -c Bio.hs

DATA_FLAGS = -funfolding-use-threshold0 -fno-strictness 
BioData.hi BioData.o : BioData.hs 
	@echo "Making BioData.hs WITHOUT optimisation (for speed)"
	${GHC} -cpp -fglasgow-exts -package lang ${DATA_FLAGS} -c $<

run : ${PROG}
	./${PROG} +RTS -K5M -RTS `cat 1-600.dna`

run2 : ${PROG}
	./${PROG} +RTS -K15M -RTS `cat 1-1200.dna`

clean :
	rm -rf ${PROG} Bio.info Bio.hs BioData.hs *.o *.hi out.daVinci 
