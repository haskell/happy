TOP = ..
include $(TOP)/Makefile.defs

FILTER = --filter
FILTER = 

.y.hs : 
	${HAPPY} -i -l $*.y ${FILTER}

expr : Expr.hs Main.lhs
	${GHC} -cpp -fglasgow-exts  -o expr --make Main.lhs

run : expr
	./expr "+ 1 1 1 1 1 1 "

eof : expr
	echo testing premature eof
	./expr ""

err : expr
	echo testing syntax error 
	./expr "+ 1 +"

test : run eof err

clean :
	rm -rf expr Expr.info Expr.hs ExprData.hs *.o *.hi out.daVinci
