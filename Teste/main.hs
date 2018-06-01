module Main where

import Lexico
import Sintatico
import System.IO
import System.Environment

main = do
   prog_name <- getArgs
   if prog_name == [] then
      putStrLn "ERRO: Faltou o nome do arquivo a ser executado!\n"
   else
      let (cmd:_) = prog_name in
         case parser (getTokens cmd) of
            Left err -> print err
            Right ans -> print ans
