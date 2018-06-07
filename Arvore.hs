module Arvore where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Data.Functor.Identity
import Text.Show.Functions
import Lexico

type ParseArgs = ParsecT [Token] () Identity

data PROGRAMA = CRIAPROG ESTRS deriving (Eq,Show)

data ESTRS = 
    INICIOESTRS [ESTR] DECS
    deriving (Eq,Show)

data DECS = 
    INICIODECS [DEC] FUNCS
    deriving (Eq,Show)

data FUNCS = 
    INICIOFUNCS [SUBPROG] MAIN
    deriving (Eq,Show)

data SUBPROG = 
    CRIAFUNC FUNC |
    CRIAOPER OPER |
    CRIAPROC PROC
    deriving (Eq,Show)

data DEC =
    NOVADEC [PONT] {-TIPO-}Token DEC_IDS
    deriving (Eq,Show)

data PONT = 
    NOVOPONT {-PONTEIRO-}Token
    deriving (Eq,Show)
    
data DEC_IDS =
    CRIAIDS [VAR_]
    deriving (Eq,Show)
    
data VAR_ =
    VAR_SEM VAR |
    VAR_COM ATRIB
    deriving (Eq,Show)

data ESTR = 
    NOVOESTR {-TIPO-}Token [DEC]
    deriving (Eq,Show)

data FUNC =
    NOVOFUNC {-ID-}Token [PARAM] [PONT] {-TIPO-}Token [STMT]
    deriving (Eq,Show)

data PROC =
    NOVOPROC {-ID-}Token [PARAM] [STMT]
    deriving (Eq,Show)

data OPER =
    NOVOOPER OP [PARAM] [PONT] {-TIPO-}Token [STMT]
    deriving (Eq,Show)

data OP = 
    NOVOAdd {-Add-}Token |
    NOVOSub {-Sub-}Token |
    NOVOMult {-Mult-}Token |
    NOVODiv {-Div-}Token |
    NOVOGeq {-Geq-}Token |
    NOVOLeq {-Leq-}Token |
    NOVODiff {-Diff-}Token |
    NOVOEqual {-Equal-}Token |
    NOVOGreat {-Great-}Token |
    NOVOLess {-Less-}Token 
    deriving (Eq,Show)
        
data PARAM = 
    NOVOPARAM [PONT] {-TIPO-}Token VAR
    deriving (Eq,Show)

data MAIN = 
    Main [STMT]
    deriving (Eq,Show)

data STMT = 
    NOVODEC DEC |
    NOVOATRIBSTMT EXPR EXPR |
    NOVOINC INC |
    NOVODECR DECR |
    NOVOCHAMADA CHAMADA |
    NOVOSE NodeSE |
    NOVOENQUANTO NodeENQUANTO |
    NOVORETORNEFUNC RETORNEFUNC |
    NOVORETORNEPROC RETORNEPROC |
    NOVOSAIA NodeSAIA |
    NOVOCONTINUE NodeCONTINUE |
    NOVODELETE NodeDELETE |
    NOVOESCREVA NodeESCREVA |
    NOVOLEIA NodeLEIA |
    NOVOBLOCO NodeBLOCO
    deriving (Eq,Show)

data RETORNEFUNC =
    CRIARETORNEF EXPR
    deriving (Eq,Show)

data RETORNEPROC =
    CRIARETORNEP
    deriving (Eq,Show)

data ATRIB =
    CRIAATRIB VAR EXPR
    deriving (Eq,Show)
    
data INC =
    CRIAINC VAR
    deriving (Eq,Show)

data DECR = 
    CRIADECR VAR
    deriving (Eq,Show)

data CHAMADA =
    CRIACHAMADA {-ID-}Token [EXPR]
    deriving (Eq,Show)

data NodeLEIA = 
    CRIALEIA [VAR]
    deriving (Eq,Show)
    
data NodeESCREVA = 
    CRIAESCREVA EXPR
    deriving (Eq,Show)

data NodeBLOCO =
    CRIABLOCO [STMT]
    deriving (Eq,Show)

data EXPR = 
    CRIAOU EXPR EXPR |
    CRIASLOWOU EXPR EXPR |
    CRIAE EXPR EXPR |
    CRIASLOWE EXPR EXPR |
    CRIALESS EXPR EXPR |
    CRIALEQ EXPR EXPR |
    CRIAEQUAL EXPR EXPR |
    CRIAGEQ EXPR EXPR |
    CRIAGREAT EXPR EXPR |
    CRIADIFF EXPR EXPR |
    CRIAADD EXPR EXPR |
    CRIASUB EXPR EXPR |
    CRIAMULT EXPR EXPR |
    CRIADIV EXPR EXPR |
    CRIAMOD EXPR EXPR |
    CRIANEG EXPR |
    CRIANOT EXPR |
    CRIATEXTO {-TEXTO-}Token |
    CRIAINT {-INTEIRO-}Token |
    CRIACARACTERE {-CARACTERE-}Token |
    CRIALOGICO {-LOGICO-}Token |
    CRIAREAL {-REAL-}Token |
    CRIAVAR VAR |
    CRIACHAMADAFUNC CHAMADA |
    CRIANOVO [PONT] {-TIPO-}Token OptionalSQBRACK |
    CRIAVALOREXPR VAL |
    CRIAPARENTESES EXPR |
    CRIACONVERSAO [PONT] {-TIPO-}Token EXPR
    deriving (Eq,Show)

data VAR =
    Var [SingleVAR]
    deriving (Eq,Show)

data VAL =
    CRIAULTVAL VAR |
    CRIASEQVAL VAL
    deriving (Eq,Show)

data SingleVAR =
     SingleVar {-ID-}Token OptionalSQBRACK
     deriving (Eq,Show)

data OptionalSQBRACK =
    OptionalSQBrack [EXPR]
    deriving (Eq,Show)

data NodeSE =
    CRIASE EXPR [STMT] OptionalSENAO
    deriving (Eq,Show)

data OptionalSENAO =
    OptionalSenao [STMT]
    deriving (Eq,Show)

data NodeENQUANTO =
    CRIAENQUANTO EXPR [STMT]
    deriving (Eq,Show)

data NodeCONTINUE = 
    CRIACONTINUE {-CONTINUE-}Token
    deriving (Eq,Show)

data NodeSAIA = 
    CRIASAIA {-SAIA-}Token
    deriving (Eq,Show)

data NodeDELETE =
    CRIADELETE VAR
    deriving (Eq,Show)

