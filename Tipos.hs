module Tipos(
    Tipo (..),
    Valor (..),
    Declaracao,
    Variavel,
    getValorInicial
) where

import Arvore
import Data.List

data Tipo = TipoAtomico String
          | TipoVetor [Integer] Tipo
          | TipoPonteiro Tipo
          | TipoEstrutura String [Declaracao]
          deriving (Eq)

instance Show Tipo where
    show (TipoAtomico s)     = s
    show (TipoVetor _ t)     = "VETOR " ++ show t
    show (TipoPonteiro t)    = "PONTEIRO " ++ show t
    show (TipoEstrutura s _) = "ESTRUTURA " ++ s

data Valor = ValorInteiro Integer
           | ValorLogico Bool
           | ValorTexto String
           | ValorCaractere Char
           | ValorReal Double
           | ValorVetor [Valor]
           | ValorPonteiro String
           | ValorEstrutura [Variavel]
           deriving (Eq)

instance Show Valor where
    show (ValorInteiro i)   = show i
    show (ValorLogico b)    = show b
    show (ValorTexto t)     = t
    show (ValorCaractere c) = show c
    show (ValorReal r)      = show r
    show (ValorVetor v)     = show v
    show (ValorPonteiro p)  = show p
    show (ValorEstrutura e) = show e

type Declaracao = (String, Tipo)

-- Nome, Tipo, Valor
type Variavel = (String, Tipo, Valor)

getValorInicial :: Tipo -> Valor 
getValorInicial (TipoAtomico "INTEIRO")    = ValorInteiro 0
getValorInicial (TipoAtomico "LOGICO")     = ValorLogico False
getValorInicial (TipoAtomico "TEXTO")      = ValorTexto ""
getValorInicial (TipoAtomico "CARACTER")   = ValorCaractere ' '
getValorInicial (TipoAtomico "REAL")       = ValorReal 0
getValorInicial (TipoPonteiro _)           = ValorPonteiro ""
getValorInicial (TipoVetor (dimensao:dimensoes) tipo) =
    if null dimensoes then
        ValorVetor $ genericReplicate dimensao (getValorInicial tipo)
    else
        ValorVetor $ genericReplicate dimensao (getValorInicial (TipoVetor dimensoes tipo))
getValorInicial (TipoEstrutura _ ((nome, tipo):decs))=
    if null decs then
        ValorEstrutura $ [(nome, tipo, getValorInicial tipo)]
    else
        ValorEstrutura $ (nome, tipo, getValorInicial tipo):(f (getValorInicial (TipoEstrutura "" decs)) )
    where 
    	f (ValorEstrutura x) = x
