.PHONY:all
all: x.permutations x.cut-width.x

x.cut-width.x: cut-width.hs
	ghc -Wall -o $@ $^

x.permutations: permutations.hs
	ghc -Wall -o $@ $^

.PHONY: clean
clean:
	rm -f *.hi *.o x.*
