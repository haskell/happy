
all : 
	make loop CMD=run

clean : 
	make loop CMD=clean

DIRS = expr-eval expr-tree nlp hidden-leftrec highly-ambiguous bio-eg \
	packing

loop :
	for d in ${DIRS}; do (cd $$d && make ${CMD}); done
