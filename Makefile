HAPPY=happy
HAPPYFLAGS=-agc

GEN = src/Parser.hs src/AttrGrammarParser.hs

all : $(GEN)

src/%.hs : src/boot/%.ly
	$(HAPPY) $(HAPPYFLAGS) $< -o $@

