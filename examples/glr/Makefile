
all : 
	make loop CMD=run

clean : 
	make loop CMD=clean

DIRS = expr-eval expr-tree expr-monad \
       hidden-leftrec highly-ambiguous packing \
       nlp bio-eg

loop :
	for d in ${DIRS}; do (cd $$d && make ${CMD}); done
