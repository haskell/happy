TOP=..
include ${TOP}/Makefile.defs

expr : Expr.hs Main.lhs
	${GHC} -cpp -fglasgow-exts -package lang -o expr --make Main.lhs

NUM=20
run : expr
	./expr +RTS -s -RTS ${NUM} | grep ^Ok
	egrep '' expr.stat > out${NUM}

run30 : 
	make run NUM=30

test : run eof err

clean :
	rm -rf expr Expr.info Expr.hs ExprData.hs *.o *.hi out.daVinci

tar :
	tar chzf aj2.tgz Expr*hs Expr*y Main*hs D*hs 

