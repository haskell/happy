TOP=..
include ${TOP}/Makefile.defs

english : English.hs Main.lhs
	${GHC} -cpp -fglasgow-exts  -o english --make Main.lhs

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
