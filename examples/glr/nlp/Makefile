.SUFFIXES: .y .hs .lhs .exe

GHC=ghc 
# -dshow-passes
HAPPY=happy

FILTER = --filter
FILTER =


.y.hs : 
	${HAPPY} -i -l $*.y ${FILTER}

english : English.hs Main.lhs DV_lhs
	${GHC} -cpp -fglasgow-exts -package lang -o english --make Main.lhs

DV_lhs :
	ln -s ../common/DaVinciTypes.hs ../common/DV_lhs .
                                                                                
run : english
	./english "the man saw the race with a telescope"

eof : english
	echo testing premature eof
	./english "the man saw a"

err : english
	echo testing syntax error 
	./english "the the man saw race"

test : run eof err

clean :
	rm -rf english English.info English.hs EnglishData.hs *.o *.hi out.daVinci
