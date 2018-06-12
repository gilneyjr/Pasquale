module Tipos(
    Tipo (..),
    Valor (..),
    Declaracao,
    Variavel,
    getValorInicial,
    getTipoFromToken,
    getValorFromToken,
    getValorInteiro,
    getTipoFromValor
) where

import Arvore
import Lexico
import Data.List
import Text.Read

data Tipo = TipoAtomico String
          | TipoVetor [Integer] Tipo
          | TipoPonteiroFim String
          | TipoPonteiroRecursivo Tipo
          | TipoEstrutura String [Declaracao]
          deriving (Eq)

instance Show Tipo where
    show (TipoAtomico s)     = s
    show (TipoVetor d t)     = "VETOR" ++ show d ++ " " ++ show t
    show (TipoPonteiroFim t) = "PONTEIRO " ++ show t
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
getTipoFromToken (TIPO _ tipo)           = TipoEstrutura tipo []

getValorFromToken :: Token -> Valor
getValorFromToken (INTEIRO _ x)    = ValorInteiro x
getValorFromToken (REAL _ x)       = ValorReal x
getValorFromToken (TEXTO _ x)      = ValorTexto x
getValorFromToken (CARACTERE _ x)  = ValorCaractere x
getValorFromToken (LOGICO _ x)     = ValorLogico x

getValorInteiro :: Valor -> Maybe Integer
getValorInteiro (ValorInteiro x) = Just x
getValorInteiro _ = Nothing

getTipoFromValor :: Valor -> Tipo
getTipoFromValor (ValorInteiro _) = TipoAtomico "INTEIRO"
getTipoFromValor (ValorLogico _) = TipoAtomico "LOGICO"
getTipoFromValor (ValorTexto _) = TipoAtomico "TEXTO"
getTipoFromValor (ValorCaractere _) = TipoAtomico "CARACTERE"
getTipoFromValor (ValorReal _) = TipoAtomico "REAL"
getTipoFromValor (ValorVetor valores) = TipoVetor (getDimensoes valores) (getTipoPrimitivoVetor valores)
--getTipoFromValor (ValorPonteiro s) = TipoPonteiro ""
getTipoFromValor (ValorEstrutura variaveis) = TipoEstrutura "" (map f variaveis)
    where f (n,t,_) = (n,t)

getDimensoes :: [Valor] -> [Integer]
getDimensoes valor@((ValorVetor valores):_) = ((genericLength valor):(getDimensoes valores))
getDimensoes valor = [genericLength valor]

getTipoPrimitivoVetor :: [Valor] -> Tipo
getTipoPrimitivoVetor ((ValorVetor valores):_) = (getTipoPrimitivoVetor valores)
getTipoPrimitivoVetor (valor:_) = getTipoFromValor valor