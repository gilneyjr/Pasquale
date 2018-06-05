all: lexico main

lexico:
	alex lexico.x
	
main:
	ghc Main.hs parser.hs parserTokens.hs lexico.hs Arvore.hs -o Pasquale && rm *.hi *.o
