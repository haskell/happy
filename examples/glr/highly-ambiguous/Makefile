.SUFFIXES: .y .hs .exe

GHC=ghc 
# -dshow-passes
HAPPY=happy

FILTER = --filter
FILTER = 

.y.hs : 
	${HAPPY} -i -l $*.y --ghc ${FILTER}

expr : Expr.hs Main.lhs
	${GHC} -cpp -fglasgow-exts -package lang -o expr --make Main.lhs

run : expr
	./expr 12 | grep ^Ok

test : run eof err

clean :
	rm -rf expr Expr.info Expr.hs ExprData.hs *.o *.hi out.daVinci

tar :
	tar chzf aj2.tgz Expr*hs Expr*y Main*hs D*hs 

