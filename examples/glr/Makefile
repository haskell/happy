
all : 
	make loop CMD=run

clean : 
	make loop CMD=clean

loop :
	for d in expr-eval expr-tree nlp bio-eg; do (cd $$d && make ${CMD}); done
