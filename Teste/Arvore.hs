module Arvore where

import Control.Monad.Identity
import Text.Parsec
import Lexico

type ParseArgs = ParsecT [Token] () Identity

data PROGRAMA = 
    CRIAPROG MAIN
    deriving (Eq,Show)

data MAIN = 
    Main [STMT]
    deriving (Eq,Show)

data DEC =
    NOVADEC {-TIPO-}Token VAR_
    deriving (Eq,Show)

data STMT = 
    NOVODEC DEC
    deriving (Eq,Show)

data VAR_ =
    VAR_SEM {-ID-} Token |
    VAR_COM ATRIB
    deriving (Eq,Show)

data ATRIB =
    CRIAATRIB {-ID-}Token EXPR
    deriving (Eq,Show)

-- Expressões

data EXPR = 
    CRIAINT {-INTEIRO-}Token |
    CRIAPARENTESES EXPR
    deriving (Eq,Show)