module Sintatico  where

import Lexico
import Text.Parsec

-- Parsers dos tokens
principalToken = tokenPrim show update_pos get_token where
   get_token (PRINCIPAL x) = Just (PRINCIPAL x)
   get_token _ = Nothing

fimPrincipalToken = tokenPrim show update_pos get_token where
   get_token (FIMPRINCIPAL x) = Just (FIMPRINCIPAL x)
   get_token _ = Nothing

escrevaToken = tokenPrim show update_pos get_token where
   get_token (ESCREVA x) = Just (ESCREVA x)
   get_token _ = Nothing

openBrackToken = tokenPrim show update_pos get_token where
   get_token (OpenBrack x) = Just (OpenBrack x)
   get_token _ = Nothing

closeBrackToken = tokenPrim show update_pos get_token where
   get_token (CloseBrack x) = Just (CloseBrack x)
   get_token _ = Nothing

textoToken = tokenPrim show update_pos get_token where
   get_token (TEXTO x y) = Just (TEXTO x y)
   get_token _ = Nothing

endCommandToken = tokenPrim show update_pos get_token where
   get_token (EndCommand x) = Just (EndCommand x)
   get_token _ = Nothing

-- Parsers dos Símbolos não terminais
program :: Parsec [Token] st [Token]
program = structs

structs = (do
            a <- struct
            b <- endCommandToken
            c <- structs
            return b ++ [c] ++ d) <|> decs

decs = (do
           a <- dec
           b <- endCommandToken
           c <- decs
           return a ++ [b] ++ c) <|> funcs

funcs = main

main = do a <- principalToken
          b <- stmts
          c <- fimPrincipalToken
          return (a:b) ++ [c]

-- Só é usado porque o Parsec pede
update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ _ = pos

-- Função que inicia o parser
parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "ERRO: " tokens

