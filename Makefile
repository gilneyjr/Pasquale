all: Lexico Main

Lexico:
	alex Lexico.x
	
Main:
	ghc Main.hs Parser.hs ParserTokens.hs Lexico.hs Arvore.hs Estado.hs Tipos.hs -o Pasquale
