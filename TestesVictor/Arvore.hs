module Arvore where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Show.Functions
import Lexico


data PROGRAMA = CRIAPROG ESTRS

data ESTRS = 
    INICIOESTRS [ESTR] DECS
    | AA Token Token --So pra testar

data DECS = 
    INICIODECS [DEC] FUNCS

data FUNCS = 
    INICIOFUNCS [SUBPROG] MAIN

data SUBPROG = 
    CRIAFUNC FUNC |
    CRIAOPER OPER |
    CRIAPROC PROC

data DEC =
    NOVADEC [MODF] {-TIPO-}Token DEC_IDS

data MODF = 
    NOVOPONT {-PONTEIRO-}Token |
    NOVOCONST {-CONST-}Token
    
data DEC_IDS =
    CRIAIDS [VAR_]
    
data VAR_ =
    VAR_SEM VAR |
    VAR_COM ATTRIB

data ESTR = 
    NOVOESTR {-TIPO-}Token [DEC]

data FUNC =
    NOVOFUNC {-ID-}Token [PARAM] {-TIPO-}Token [STMT]

data PROC =
    NOVOPROC {-ID-}Token [PARAM] [STMT]

data OPER =
    NOVOOPER OP [PARAM] {-TIPO-}Token [STMT]

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
        
data PARAM = 
    NOVOPARAM {-TIPO-}Token VAR

data MAIN = 
    Main [STMT]

data STMT = 
    NOVODEC DEC |
    NOVOATTRIB ATTRIB |
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
    NOVOES ES |
    NOVOBLOCO NodeBLOCO

data RETORNEFUNC =
    CRIARETORNEF EXPR

data RETORNEPROC =
    CRIARETORNEP

data ATTRIB =
    CRIAATTRIB VAR EXPR
    
data INC =
    CRIAINC VAR

data DECR = 
    CRIADECR VAR

data CHAMADA =
    CRIACHAMADA {-ID-}Token [EXPR]

data ES = 
    NOVOLEIA NodeLEIA |
    NOVOESCREVA NodeESCREVA

data NodeLEIA = 
    CRIALEIA [VAR]
    
data NodeESCREVA = 
    CRIAESCREVA EXPR

data NodeBLOCO =
    CRIABLOCO [STMT]

data EXPR = 
    CRIAOU EXPR EXPR |
    CRIAOU_ EXPR EXPR |
    CRIAE EXPR EXPR |
    CRIAE_ EXPR EXPR |
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
    CRIACARACTERE {-CARACETERE-}Token |
    CRIALOGICO {-LOGICO-}Token |
    CRIAREAL {-REAL-}Token |
    CRIAVAR VAR |
    CRIACHAMADAFUNC CHAMADA |
    CRIANOVO {-TIPO-}Token [EXPR] |
    CRIACONVERSAO {-TIPO-}Token EXPR

data VAR =
    Var {-ID-}Token [EXPR]
    
data NodeSE =
    CRIASE EXPR [STMT] [STMT]

data NodeENQUANTO =
    CRIAENQUANTO EXPR [STMT]

data NodeCONTINUE = 
    CRIACONTINUE {-CONTINUE-}Token

data NodeSAIA = 
    CRIASAIA {-SAIA-}Token

data NodeDELETE =
    CRIADELETE {-DELETE-}Token VAR

