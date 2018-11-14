# Pasquale

## Description
Interpreter for an imperative programming language, Pasquale, developed in Haskell. Pasquale is lexically scoped and supports arrays, blocks, conditional statements, loops, pointers, user-defined (possibly recursive) types and subroutines (procedures / functions / operator overloading). Because of its educational purposes, all the commands and error messages were written in Portuguese.

## Requirements

* GHC/GHCI (Haskell compiler/interpreter):
   * Download: https://www.haskell.org/downloads
* Parsec (Parser library):
   * Run **# cabal install parsec** to install.
   * For more information, visit http://hackage.haskell.org/package/parsec.
* Alex (Lexical analyser generator):
   * Run **# cabal install alex** to install.
   * For more information, visit https://www.haskell.org/alex/#Download.

## Compiling and running
To compile, enter `Pasquale/` and run the **$ make** command on Terminal.

To run the Interpreter, in the same folder run **$ ./Pasquale FileName**. For example: **$ ./Pasquale Problemas/Problema1.txt**.


## Descrição
Desenvolvimento de um interpretador para uma linguagem de programação no paradigma imperativo feita em Haskell.

## Requisitos

* Possuir o GHC/GHCI (respectivamente compilador e interpretador Haskell) instalados em seu computador.
   * Link: https://www.haskell.org/downloads
* Possuir o módulo Parsec (do Haskell) instalado em seu computador.
   * Use o comando **# cabal install parsec** em seu terminal.
   * Para mais informações, acesse http://hackage.haskell.org/package/parsec.
* Possuir o Alex instalado em seu computador.
   * Use o comando **# cabal install alex** em seu terminal.
   * Para mais informações, acesse https://www.haskell.org/alex/#Download.

## Compilação e execução
Para compilar, basta executar o MakeFile, digitando o comando **$ make** em seu terminal. Para a execução do interpretador, basta executar **$ ./Pasquale nome_do_arquivo**. Por exemplo: **$ ./Pasquale Problemas/Problema1.txt**.
