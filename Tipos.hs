module Tipos(
    Tipo (..),
    Valor (..),
    Declaracao,
    Variavel
) where

import Arvore

data Tipo = TipoAtomico String
          | TipoVetor Tipo
          | TipoPonteiro Tipo
          | TipoEstrutura String [Declaracao]
          deriving (Eq)

instance Show Tipo where
    show (TipoAtomico s)     = s
    show (TipoVetor t)       = "VETOR " ++ show t
    show (TipoPonteiro t)    = "PONTEIRO " ++ show t
    show (TipoEstrutura s t) = "ESTRUTURA " ++ s

data Valor = ValorInteiro Integer
           | ValorLogico Bool
           | ValorTexto String
           | ValorCaractere Char
           | ValorReal Double
           | ValorVetor [Valor]
           | ValorEstrutura [Variavel]
           deriving (Eq)

instance Show Valor where
    show (ValorInteiro i)   = show i
    show (ValorLogico b)    = show b
    show (ValorTexto t)     = t
    show (ValorCaractere c) = show c
    show (ValorReal r)      = show r
    show (ValorVetor v)     = show v
    show (ValorEstrutura e) = show e

type Declaracao = (String, Tipo)

-- Nome, Tipo, Valor
type Variavel = (String, Tipo, Valor)