module Tipos(
    Tipo (..),
    Valor (..),
    Declaracao,
    Variavel,
    getValorInicial,
    getTipoFromToken,
    getValorFromToken
) where

import Arvore
import Lexico
import Data.List

data Tipo = TipoAtomico String
          | TipoVetor [Integer] Tipo
          | TipoPonteiroFim String
          | TipoPonteiroRecursivo Tipo
          | TipoEstrutura String [Declaracao]
          deriving (Eq)

instance Show Tipo where
    show (TipoAtomico s)     = s
    show (TipoVetor _ t)     = "VETOR " ++ show t
    show (TipoPonteiroFim t) = "PONTEIRO" ++ show t
    show (TipoPonteiroRecursivo t)    = "PONTEIRO " ++ show t
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
getValorInicial (TipoPonteiroFim _)           = ValorPonteiro ""
getValorInicial (TipoPonteiroRecursivo _)           = ValorPonteiro ""
getValorInicial (TipoVetor (dimensao:dimensoes) tipo) =
    if null dimensoes then
        ValorVetor $ genericReplicate dimensao (getValorInicial tipo)
    else
        ValorVetor $ genericReplicate dimensao (getValorInicial (TipoVetor dimensoes tipo))
getValorInicial (TipoEstrutura _ ((nome, tipo):decs))=
    if null decs then
        ValorEstrutura $ [(nome, tipo, getValorInicial tipo)]
    else
        ValorEstrutura $ (nome, tipo, getValorInicial tipo):valores
    where 
        (ValorEstrutura valores) = (getValorInicial (TipoEstrutura "" decs))

getTipoFromToken :: Token -> Tipo
getTipoFromToken (INTEIRO _ _)    = TipoAtomico "INTEIRO"
getTipoFromToken (REAL _ _)       = TipoAtomico "REAL"
getTipoFromToken (CARACTERE _ _)  = TipoAtomico "CARACTERE"
getTipoFromToken (TEXTO _ _)      = TipoAtomico "TEXTO"
getTipoFromToken (LOGICO _ _)     = TipoAtomico "LOGICO"

getTipoFromToken (TIPO _ "INTEIRO")      = TipoAtomico "INTEIRO"
getTipoFromToken (TIPO _ "REAL")         = TipoAtomico "REAL"
getTipoFromToken (TIPO _ "CARACTERE")    = TipoAtomico "CARACTERE"
getTipoFromToken (TIPO _ "TEXTO")        = TipoAtomico "TEXTO"
getTipoFromToken (TIPO _ "LOGICO")       = TipoAtomico "LOGICO"

getValorFromToken (INTEIRO _ x)    = ValorInteiro x
getValorFromToken (REAL _ x)       = ValorReal x
getValorFromToken (TEXTO _ x)      = ValorTexto x
getValorFromToken (CARACTERE _ x)  = ValorCaractere x
getValorFromToken (LOGICO _ x)     = ValorLogico x


