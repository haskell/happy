
all:
	alex Lexer.x
	happy Parser.y
	ghc --make Foo -o foo

test:
	echo fork leaf leaf | ./foo
	echo fork leaf leafqleaf | ./foo
	echo leaf leaf leaf leaf leaf | ./foo

clean:
	rm -f *.o *.hi Parser.hs Lexer.hs foo

