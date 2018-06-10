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
    NOVADEC [PONT] {-TIPO-}Token [VAR_]
    deriving (Eq,Show)

data PONT = 
    NOVOPONT {-PONTEIRO-}Token
    deriving (Eq,Show)
    
data VAR_ =
    VAR_SEM SingleVAR |
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
    NOVOPARAM [PONT] {-TIPO-}Token SingleVAR
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
    CRIARETORNEF {-RETORNE-}Token EXPR
    deriving (Eq,Show)

data RETORNEPROC =
    CRIARETORNEP {-RETORNE-}Token
    deriving (Eq,Show)

data ATRIB =
    CRIAATRIB SingleVAR EXPR
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
    CRIALEIA {-LEIA-}Token [VAR]
    deriving (Eq,Show)
    
data NodeESCREVA = 
    CRIAESCREVA {-ESCREVA-}Token EXPR
    deriving (Eq,Show)

data NodeBLOCO =
    CRIABLOCO [STMT]
    deriving (Eq,Show)

data EXPR = 
    CRIAOU EXPR Token EXPR |
    CRIASLOWOU EXPR Token EXPR |
    CRIAE EXPR Token EXPR |
    CRIASLOWE EXPR Token EXPR |
    CRIALESS EXPR Token EXPR |
    CRIALEQ EXPR Token EXPR |
    CRIAEQUAL EXPR Token EXPR |
    CRIAGEQ EXPR Token EXPR |
    CRIAGREAT EXPR Token EXPR |
    CRIADIFF EXPR Token EXPR |
    CRIAADD EXPR Token EXPR |
    CRIASUB EXPR Token EXPR |
    CRIAMULT EXPR Token EXPR |
    CRIADIV EXPR Token EXPR |
    CRIAMOD EXPR Token EXPR |
    CRIANEG Token EXPR |
    CRIANOT Token EXPR |
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
    CRIACONVERSAO {-TIPO-}Token EXPR
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
    CRIASE Token EXPR [STMT] OptionalSENAO
    deriving (Eq,Show)

data OptionalSENAO =
    OptionalSenao [STMT]
    deriving (Eq,Show)

data NodeENQUANTO =
    CRIAENQUANTO {-ENQUANTO-}Token EXPR [STMT]
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

