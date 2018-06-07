all: lexico main

Lexico:
	alex Lexico.x
	
Main:
	ghc Main.hs Parser.hs ParserTokens.hs Lexico.hs Arvore.hs -o Pasquale && rm *.hi *.o
