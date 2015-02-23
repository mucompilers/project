all: clean
	mkdir -p bin build
	ghc --make src/main.hs src/language.hs src/lexer.hs src/parser.hs \
	    -o bin/hw0 \
	    -odir build \
	    -hidir build \
	    -O2

clean:
	rm -rf bin build
