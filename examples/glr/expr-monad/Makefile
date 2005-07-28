TOP=..
include ${TOP}/Makefile.defs

DECODE = --decode


expr : Expr.hs Main.lhs 
	${GHC} -cpp -fglasgow-exts -package lang -o expr --make Main.lhs

run : expr
	./expr "1+2*4-3"

eof : expr
	echo testing premature eof
	./expr "1+2*"

err : expr
	echo testing syntax error 
	./expr "1+2*2++3"

test : run eof err

clean :
	rm -rf expr Expr.info Expr.hs ExprData.hs *.o *.hi out.daVinci 
