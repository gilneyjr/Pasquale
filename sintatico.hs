module Sintatico (main) where

import Lexico
import Text.Parsec

-- Parsers dos tokens

estruturaToken = tokenPrim show update_pos get_token where
  get_token ESTRUTURA = Just ESTRUTURA
  get_token _         = Nothing

fimEstruturaToken = tokenPrim show update_pos get_token where
  get_token FIMESTRUTURA = Just FIMESTRUTURA
  get_token _            = Nothing

funcaoToken = tokenPrim show update_pos get_token where
  get_token FUNCAO = Just FUNCAO
  get_token _      = Nothing

fimFuncaoToken = tokenPrim show update_pos get_token where
  get_token FIMFUNCAO = Just FIMFUNCAO
  get_token _         = Nothing

procedimentoToken = tokenPrim show update_pos get_token where
  get_token PROCEDIMENTO = Just PROCEDIMENTO
  get_token _            = Nothing

fimProcedimentoToken = tokenPrim show update_pos get_token where
  get_token FIMPROCEDIMENTO = Just FIMPROCEDIMENTO
  get_token _               = Nothing

operadorToken = tokenPrim show update_pos get_token where
  get_token OPERADOR = Just OPERADOR
  get_token _        = Nothing

fimOperadorToken = tokenPrim show update_pos get_token where
  get_token FIMOPERADOR = Just FIMOPERADOR
  get_token _           = Nothing

recebeToken = tokenPrim show update_pos get_token where
  get_token RECEBE = Just RECEBE
  get_token _      = Nothing

retornaToken = tokenPrim show update_pos get_token where
  get_token RETORNA = Just RETORNA
  get_token _       = Nothing

retorneToken = tokenPrim show update_pos get_token where
  get_token RETORNE = Just RETORNE
  get_token _       = Nothing 

principalToken = tokenPrim show update_pos get_token where
  get_token PRINCIPAL = Just PRINCIPAL
  get_token _         = Nothing

fimPrincipalToken = tokenPrim show update_pos get_token where
  get_token FIMPRINCIPAL = Just FIMPRINCIPAL
  get_token _            = Nothing

saiaToken = tokenPrim show update_pos get_token where
  get_token SAIA = Just SAIA
  get_token _    = Nothing

continueToken = tokenPrim show update_pos get_token where
  get_token CONTINUE = Just CONTINUE
  get_token _        = Nothing

seToken = tokenPrim show update_pos get_token where
  get_token SE = Just SE
  get_token _  = Nothing

entaoToken = tokenPrim show update_pos get_token where
  get_token ENTAO = Just ENTAO
  get_token _     = Nothing

fimSeToken = tokenPrim show update_pos get_token where
  get_token FIMSE = Just FIMSE
  get_token _     = Nothing

enquantoToken = tokenPrim show update_pos get_token where
  get_token ENQUANTO = Just ENQUANTO
  get_token _        = Nothing

executeToken = tokenPrim show update_pos get_token where
  get_token EXECUTE = Just EXECUTE
  get_token _       = Nothing

fimEnquantoToken = tokenPrim show update_pos get_token where
  get_token FIMENQUANTO = Just FIMENQUANTO
  get_token _           = Nothing

definaToken = tokenPrim show update_pos get_token where
  get_token DEFINA = Just DEFINA
  get_token _      = Nothing

slowOUToken = tokenPrim show update_pos get_token where
  get_token SlowOU = Just SlowOU
  get_token _      = Nothing

slowEToken = tokenPrim show update_pos get_token where
  get_token SlowE = Just SlowE
  get_token _     = Nothing

ouToken = tokenPrim show update_pos get_token where
  get_token OU = Just OU
  get_token _  = Nothing

eToken = tokenPrim show update_pos get_token where
  get_token E = Just E
  get_token _ = Nothing

boolValueToken = tokenPrim show update_pos get_token where
  get_token BoolValue = Just BoolValue
  get_token _         = Nothing

pontToken = tokenPrim show update_pos get_token where
  get_token PONT = Just PONT
  get_token _    = Nothing

novoToken = tokenPrim show update_pos get_token where
  get_token NOVO = Just NOVO
  get_token _    = Nothing

deleteToken = tokenPrim show update_pos get_token where
  get_token DELETE = Just DELETE
  get_token _      = Nothing

constToken = tokenPrim show update_pos get_token where
  get_token CONST = Just CONST
  get_token _     = Nothing

attribToken = tokenPrim show update_pos get_token where
  get_token Attrib = Just Attrib
  get_token _      = Nothing

geqToken = tokenPrim show update_pos get_token where
  get_token Geq = Just Geq
  get_token _   = Nothing

leqToken = tokenPrim show update_pos get_token where
  get_token Leq = Just Leq
  get_token _   = Nothing

diffToken = tokenPrim show update_pos get_token where
  get_token Diff = Just Diff
  get_token _    = Nothing

equalToken = tokenPrim show update_pos get_token where
  get_token Equal = Just Equal
  get_token _     = Nothing

greatToken = tokenPrim show update_pos get_token where
  get_token Great = Just Great
  get_token _     = Nothing

lessToken = tokenPrim show update_pos get_token where
  get_token Less = Just Less
  get_token _    = Nothing

addToken = tokenPrim show update_pos get_token where
  get_token Add = Just Add
  get_token _   = Nothing

subToken = tokenPrim show update_pos get_token where
  get_token Sub = Just Sub
  get_token _   = Nothing

multToken = tokenPrim show update_pos get_token where
  get_token Mult = Just Mult
  get_token _    = Nothing

divToken = tokenPrim show update_pos get_token where
  get_token Div = Just Div
  get_token _   = Nothing

modToken = tokenPrim show update_pos get_token where
  get_token MOD = Just MOD
  get_token _   = Nothing

notToken = tokenPrim show update_pos get_token where
  get_token NOT = Just NOT
  get_token _   = Nothing

openBrackToken = tokenPrim show update_pos get_token where
  get_token OpenBrack = Just OpenBrack
  get_token _         = Nothing

closeBrackToken = tokenPrim show update_pos get_token where
  get_token CloseBrack = Just CloseBrack
  get_token _          = Nothing

commaToken = tokenPrim show update_pos get_token where
  get_token Comma = Just Comma
  get_token _     = Nothing

endCommandToken = tokenPrim show update_pos get_token where
  get_token EndCommand = Just EndCommand
  get_token _          = Nothing

inteiroToken = tokenPrim show update_pos get_token where
  get_token INTEIRO = Just INTEIRO
  get_token _       = Nothing

textoToken = tokenPrim show update_pos get_token where
  get_token TEXTO = Just TEXTO
  get_token _     = Nothing

tipoToken = tokenPrim show update_pos get_token where
  get_token TIPO = Just TIPO
  get_token _    = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token ID = Just ID
  get_token _  = Nothing

  