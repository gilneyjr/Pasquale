all: lexico main

lexico:
	alex lexico.x
	
main:
	ghc Main.hs Parser.hs ParserTokens.hs Lexico.hs Arvore.hs -o Pasquale && rm *.hi *.o
