TOP=..
include ${TOP}/Makefile.defs

PROG=bio-eg

# filtering causes this example to fail...
FILTER = --filter
FILTER =

.hi.o :
	@ dummy

${PROG} : Bio.o Main.lhs
	${GHC} -cpp -fglasgow-exts  -o ${PROG} --make Main.lhs

BioData.hs Bio.hs : Bio.y
	${HAPPY} --info --glr --ghc ${FILTER} $<

Bio.o : Bio.hs BioData.hi
	${GHC} -cpp -fglasgow-exts  -O2 -c Bio.hs

DATA_FLAGS = -funfolding-use-threshold0 -fno-strictness
BioData.hi BioData.o : BioData.hs
	@echo "Making BioData.hs WITHOUT optimisation (for speed)"
	${GHC} -cpp -fglasgow-exts  ${DATA_FLAGS} -c $<

run : run12

run6 : ${PROG}
	./${PROG} +RTS -s -K5M -RTS `cat 1-600.dna`

run12 : ${PROG}
	rm -f out.1200
	./${PROG} +RTS -s -K15M -RTS `cat 1-1200.dna` >& out.1200
	echo Expect NINE matches, got `grep '^[(,0-9]*G_Match' out.1200 | wc -l`

clean :
	rm -rf ${PROG} Bio.info Bio.hs BioData.hs *.o *.hi out.daVinci \
		out.1200 out.600
