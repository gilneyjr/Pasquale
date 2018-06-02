module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Data.Functor.Identity
import Control.Monad
import Arvore
import ParserTokens
import Lexico

parsePasquale :: String -> Either ParseError PROGRAMA
parsePasquale input = runParser parsePrograma () "" (getTokens input)

parsePrograma :: ParseArgs PROGRAMA
parsePrograma = do
    a <- parseEstrs
    return $ CRIAPROG a 

parseEstrs :: ParseArgs ESTRS
parseEstrs = do
    a <- many parseEstr
    b <- parseDecs
    return $ INICIOESTRS a  b 

parseDecs :: ParseArgs DECS
parseDecs = do
    a <- many parseDec
    b <- parseFuncs
    return $ INICIODECS a  b 

parseFuncs :: ParseArgs FUNCS
parseFuncs = do
    a <- many parseSubprog
    b <- parseMain
    return $ INICIOFUNCS a  b 

parseSubprog :: ParseArgs SUBPROG
parseSubprog = 
    (try parseCriafunc) <|>
    (try parseCriaoper) <|>
    parseCriaproc

parseCriafunc :: ParseArgs SUBPROG
parseCriafunc = do
    a <- parseFunc
    return $ CRIAFUNC a 

parseCriaoper :: ParseArgs SUBPROG
parseCriaoper = do
    a <- parseOper
    return $ CRIAOPER a 

parseCriaproc :: ParseArgs SUBPROG
parseCriaproc = do
    a <- parseProc
    return $ CRIAPROC a 

parseDec :: ParseArgs DEC
parseDec = do
    a <- many parseModf
    b <- parseTipo
    c <- parseDec_ids
    return $ NOVADEC a  b  c 

parseModf :: ParseArgs MODF
parseModf = 
    (try parseNovopont) <|>
    parseNovoconst

parseNovopont :: ParseArgs MODF
parseNovopont = do
    a <- parsePont
    return $ NOVOPONT a 

parseNovoconst :: ParseArgs MODF
parseNovoconst = do
    a <- parseConst
    return $ NOVOCONST a 

parseDec_ids :: ParseArgs DEC_IDS
parseDec_ids = do
    a <- many parseVar_
    return $ CRIAIDS a 

parseVar_ :: ParseArgs VAR_
parseVar_ = 
    (try parseVar_sem) <|>
    parseVar_com

parseVar_sem :: ParseArgs VAR_
parseVar_sem = do
    a <- parseVar
    return $ VAR_SEM a 

parseVar_com :: ParseArgs VAR_
parseVar_com = do
    a <- parseAtrib
    return $ VAR_COM a 

parseEstr :: ParseArgs ESTR
parseEstr = do
    a <- parseTipo
    b <- many parseDec
    return $ NOVOESTR a  b 

parseFunc :: ParseArgs FUNC
parseFunc = do
    a <- parseId
    b <- many parseParam
    c <- parseTipo
    d <- many parseStmt
    return $ NOVOFUNC a  b  c  d 

parseProc :: ParseArgs PROC
parseProc = do
    a <- parseId
    b <- many parseParam
    c <- many parseStmt
    return $ NOVOPROC a  b  c 

parseOper :: ParseArgs OPER
parseOper = do
    a <- parseOp
    b <- many parseParam
    c <- parseTipo
    d <- many parseStmt
    return $ NOVOOPER a  b  c  d 

parseOp :: ParseArgs OP
parseOp = 
    (try parseNovoadd) <|>
    (try parseNovosub) <|>
    (try parseNovomult) <|>
    (try parseNovodiv) <|>
    (try parseNovogeq) <|>
    (try parseNovoleq) <|>
    (try parseNovodiff) <|>
    (try parseNovoequal) <|>
    (try parseNovogreat) <|>
    parseNovoless

parseNovoadd :: ParseArgs OP
parseNovoadd = do
    a <- parseAdd
    return $ NOVOAdd a 

parseNovosub :: ParseArgs OP
parseNovosub = do
    a <- parseSub
    return $ NOVOSub a 

parseNovomult :: ParseArgs OP
parseNovomult = do
    a <- parseMult
    return $ NOVOMult a 

parseNovodiv :: ParseArgs OP
parseNovodiv = do
    a <- parseDiv
    return $ NOVODiv a 

parseNovogeq :: ParseArgs OP
parseNovogeq = do
    a <- parseGeq
    return $ NOVOGeq a 

parseNovoleq :: ParseArgs OP
parseNovoleq = do
    a <- parseLeq
    return $ NOVOLeq a 

parseNovodiff :: ParseArgs OP
parseNovodiff = do
    a <- parseDiff
    return $ NOVODiff a 

parseNovoequal :: ParseArgs OP
parseNovoequal = do
    a <- parseEqual
    return $ NOVOEqual a 

parseNovogreat :: ParseArgs OP
parseNovogreat = do
    a <- parseGreat
    return $ NOVOGreat a 

parseNovoless :: ParseArgs OP
parseNovoless = do
    a <- parseLess
    return $ NOVOLess a 

parseParam :: ParseArgs PARAM
parseParam = do
    a <- parseTipo
    b <- parseVar
    return $ NOVOPARAM a  b 

parseMain :: ParseArgs MAIN
parseMain = do
    a <- many parseStmt
    return $ Main a 

parseStmt :: ParseArgs STMT
parseStmt = 
    (try parseNovodec) <|>
    (try parseNovoatrib) <|>
    (try parseNovoinc) <|>
    (try parseNovodecr) <|>
    (try parseNovochamada) <|>
    (try parseNovose) <|>
    (try parseNovoenquanto) <|>
    (try parseNovoretornefunc) <|>
    (try parseNovoretorneproc) <|>
    (try parseNovosaia) <|>
    (try parseNovocontinue) <|>
    (try parseNovodelete) <|>
    (try parseNovoes) <|>
    parseNovobloco

parseNovodec :: ParseArgs STMT
parseNovodec = do
    a <- parseDec
    return $ NOVODEC a 

parseNovoatrib :: ParseArgs STMT
parseNovoatrib = do
    a <- parseAtrib
    return $ NOVOATRIB a 

parseNovoinc :: ParseArgs STMT
parseNovoinc = do
    a <- parseInc
    return $ NOVOINC a 

parseNovodecr :: ParseArgs STMT
parseNovodecr = do
    a <- parseDecr
    return $ NOVODECR a 

parseNovochamada :: ParseArgs STMT
parseNovochamada = do
    a <- parseChamada
    return $ NOVOCHAMADA a 

parseNovose :: ParseArgs STMT
parseNovose = do
    a <- parseNodese
    return $ NOVOSE a 

parseNovoenquanto :: ParseArgs STMT
parseNovoenquanto = do
    a <- parseNodeenquanto
    return $ NOVOENQUANTO a 

parseNovoretornefunc :: ParseArgs STMT
parseNovoretornefunc = do
    a <- parseRetornefunc
    return $ NOVORETORNEFUNC a 

parseNovoretorneproc :: ParseArgs STMT
parseNovoretorneproc = do
    a <- parseRetorneproc
    return $ NOVORETORNEPROC a 

parseNovosaia :: ParseArgs STMT
parseNovosaia = do
    a <- parseNodesaia
    return $ NOVOSAIA a 

parseNovocontinue :: ParseArgs STMT
parseNovocontinue = do
    a <- parseNodecontinue
    return $ NOVOCONTINUE a 

parseNovodelete :: ParseArgs STMT
parseNovodelete = do
    a <- parseNodedelete
    return $ NOVODELETE a 

parseNovoes :: ParseArgs STMT
parseNovoes = do
    a <- parseEs
    return $ NOVOES a 

parseNovobloco :: ParseArgs STMT
parseNovobloco = do
    a <- parseNodebloco
    return $ NOVOBLOCO a 

parseRetornefunc :: ParseArgs RETORNEFUNC
parseRetornefunc = do
    a <- parseExpr
    return $ CRIARETORNEF a 

parseRetorneproc :: ParseArgs RETORNEPROC
parseRetorneproc = do
    return $ CRIARETORNEP

parseAtrib :: ParseArgs ATRIB
parseAtrib = do
    a <- parseVar
    b <- parseExpr
    return $ CRIAATRIB a  b 

parseInc :: ParseArgs INC
parseInc = do
    a <- parseVar
    return $ CRIAINC a 

parseDecr :: ParseArgs DECR
parseDecr = do
    a <- parseVar
    return $ CRIADECR a 

parseChamada :: ParseArgs CHAMADA
parseChamada = do
    a <- parseId
    b <- many parseExpr
    return $ CRIACHAMADA a  b 

parseEs :: ParseArgs ES
parseEs = 
    (try parseNovoleia) <|>
    parseNovoescreva

parseNovoleia :: ParseArgs ES
parseNovoleia = do
    a <- parseNodeleia
    return $ NOVOLEIA a 

parseNovoescreva :: ParseArgs ES
parseNovoescreva = do
    a <- parseNodeescreva
    return $ NOVOESCREVA a 

parseNodeleia :: ParseArgs NodeLEIA
parseNodeleia = do
    a <- many parseVar
    return $ CRIALEIA a 

parseNodeescreva :: ParseArgs NodeESCREVA
parseNodeescreva = do
    a <- parseExpr
    return $ CRIAESCREVA a 

parseNodebloco :: ParseArgs NodeBLOCO
parseNodebloco = do
    a <- many parseStmt
    return $ CRIABLOCO a 

parseExpr :: ParseArgs EXPR
parseExpr = 
    (try parseCriaou) <|>
    (try parseCriaou_) <|>
    (try parseCriae) <|>
    (try parseCriae_) <|>
    (try parseCrialess) <|>
    (try parseCrialeq) <|>
    (try parseCriaequal) <|>
    (try parseCriageq) <|>
    (try parseCriagreat) <|>
    (try parseCriadiff) <|>
    (try parseCriaadd) <|>
    (try parseCriasub) <|>
    (try parseCriamult) <|>
    (try parseCriadiv) <|>
    (try parseCriamod) <|>
    (try parseCrianeg) <|>
    (try parseCrianot) <|>
    (try parseCriatexto) <|>
    (try parseCriaint) <|>
    (try parseCriacaractere) <|>
    (try parseCrialogico) <|>
    (try parseCriareal) <|>
    (try parseCriavar) <|>
    (try parseCriachamadafunc) <|>
    (try parseCrianovo) <|>
    parseCriaconversao

parseCriaou :: ParseArgs EXPR
parseCriaou = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIAOU a  b 

parseCriaou_ :: ParseArgs EXPR
parseCriaou_ = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIAOU_ a  b 

parseCriae :: ParseArgs EXPR
parseCriae = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIAE a  b 

parseCriae_ :: ParseArgs EXPR
parseCriae_ = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIAE_ a  b 

parseCrialess :: ParseArgs EXPR
parseCrialess = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIALESS a  b 

parseCrialeq :: ParseArgs EXPR
parseCrialeq = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIALEQ a  b 

parseCriaequal :: ParseArgs EXPR
parseCriaequal = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIAEQUAL a  b 

parseCriageq :: ParseArgs EXPR
parseCriageq = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIAGEQ a  b 

parseCriagreat :: ParseArgs EXPR
parseCriagreat = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIAGREAT a  b 

parseCriadiff :: ParseArgs EXPR
parseCriadiff = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIADIFF a  b 

parseCriaadd :: ParseArgs EXPR
parseCriaadd = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIAADD a  b 

parseCriasub :: ParseArgs EXPR
parseCriasub = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIASUB a  b 

parseCriamult :: ParseArgs EXPR
parseCriamult = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIAMULT a  b 

parseCriadiv :: ParseArgs EXPR
parseCriadiv = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIADIV a  b 

parseCriamod :: ParseArgs EXPR
parseCriamod = do
    a <- parseExpr
    b <- parseExpr
    return $ CRIAMOD a  b 

parseCrianeg :: ParseArgs EXPR
parseCrianeg = do
    a <- parseExpr
    return $ CRIANEG a 

parseCrianot :: ParseArgs EXPR
parseCrianot = do
    a <- parseExpr
    return $ CRIANOT a 

parseCriatexto :: ParseArgs EXPR
parseCriatexto = do
    a <- parseTexto
    return $ CRIATEXTO a 

parseCriaint :: ParseArgs EXPR
parseCriaint = do
    a <- parseInteiro
    return $ CRIAINT a 

parseCriacaractere :: ParseArgs EXPR
parseCriacaractere = do
    a <- parseCaractere
    return $ CRIACARACTERE a 

parseCrialogico :: ParseArgs EXPR
parseCrialogico = do
    a <- parseLogico
    return $ CRIALOGICO a 

parseCriareal :: ParseArgs EXPR
parseCriareal = do
    a <- parseReal
    return $ CRIAREAL a 

parseCriavar :: ParseArgs EXPR
parseCriavar = do
    a <- parseVar
    return $ CRIAVAR a 

parseCriachamadafunc :: ParseArgs EXPR
parseCriachamadafunc = do
    a <- parseChamada
    return $ CRIACHAMADAFUNC a 

parseCrianovo :: ParseArgs EXPR
parseCrianovo = do
    a <- parseTipo
    b <- many parseExpr
    return $ CRIANOVO a  b 

parseCriaconversao :: ParseArgs EXPR
parseCriaconversao = do
    a <- parseTipo
    b <- parseExpr
    return $ CRIACONVERSAO a  b 

parseVar :: ParseArgs VAR
parseVar = do
    a <- parseId
    b <- many parseExpr
    return $ Var a  b 

parseNodese :: ParseArgs NodeSE
parseNodese = do
    a <- parseExpr
    b <- many parseStmt
    c <- many parseStmt
    return $ CRIASE a  b  c 

parseNodeenquanto :: ParseArgs NodeENQUANTO
parseNodeenquanto = do
    a <- parseExpr
    b <- many parseStmt
    return $ CRIAENQUANTO a  b 

parseNodecontinue :: ParseArgs NodeCONTINUE
parseNodecontinue = do
    a <- parseContinue
    return $ CRIACONTINUE a 

parseNodesaia :: ParseArgs NodeSAIA
parseNodesaia = do
    a <- parseSaia
    return $ CRIASAIA a 

parseNodedelete :: ParseArgs NodeDELETE
parseNodedelete = do
    a <- parseDelete
    b <- parseVar
    return $ CRIADELETE a  b 

