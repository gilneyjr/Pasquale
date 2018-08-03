# Pasquale
## Descrição
Desenvolvimento de um interpretador para uma linguagem de programação no paradigma imperativo feita em Haskell.

## Requisitos

* Possuir o GHC/GHCI (respectivamente compilador e interpretador Haskell) instalados em seu computador
   * Link: https://www.haskell.org/downloads
* Possuir o módulo Parsec (do Haskell) instalado em seu computador.
   * Use o comando **# cabal install parsec** em seu terminal.
   * Para mais informações, acesse http://hackage.haskell.org/package/parsec.
* Possuir o Alex instalado em seu computador.
   * Use o comando **# cabal install alex** em seu terminal.
   * Para mais informações, acesse https://www.haskell.org/alex/#Download.

## Compilação e execução
Para compilar, basta executar o MakeFile, digitando o comando **$ make** em seu terminal. Para a execução do interpretador, basta executar **$ ./Pasquale nome_do_arquivo**. Por exemplo: **$ ./Pasquale Problemas/Problema1.txt**.