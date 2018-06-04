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
    return $ INICIOESTRS a b 

parseDecs :: ParseArgs DECS
parseDecs = do
    a <- endBy parseDec parseEndcommand
    b <- parseFuncs
    return $ INICIODECS a b 

parseFuncs :: ParseArgs FUNCS
parseFuncs = do
    a <- many parseSubprog
    b <- parseMain
    return $ INICIOFUNCS a b 

parseSubprog :: ParseArgs SUBPROG
parseSubprog = 
    parseCriafunc <|>
    parseCriaoper <|>
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
    return $ NOVADEC a b c 

parseModf :: ParseArgs MODF
parseModf = 
    parseNovopont <|>
    parseNovoconst

parseNovopont :: ParseArgs MODF
parseNovopont = do
    a <- parsePonteiro
    return $ NOVOPONT a 

parseNovoconst :: ParseArgs MODF
parseNovoconst = do
    a <- parseConst
    return $ NOVOCONST a 

parseDec_ids :: ParseArgs DEC_IDS
parseDec_ids = do
    a <- sepBy parseVar_ parseComma
    return $ CRIAIDS a 

parseVar_ :: ParseArgs VAR_
parseVar_ = 
    (try parseVar_com) <|>
    parseVar_sem

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
    parseEstrutura
    a <- parseTipo
    b <- endBy parseDec parseEndcommand
    parseFimestrutura
    return $ NOVOESTR a b

parseFunc :: ParseArgs FUNC
parseFunc = do
    parseFuncao
    a <- parseId
    parseRecebe
    parseOpenbrack
    b <- sepBy parseParam parseComma
    parseClosebrack
    parseRetorna
    c <- many parseModf
    d <- parseTipo
    e <- many parseStmt
    parseFimfuncao
    return $ NOVOFUNC a b c d e

parseProc :: ParseArgs PROC
parseProc = do
    parseProcedimento
    a <- parseId
    parseRecebe
    parseOpenbrack
    b <- sepBy parseParam parseComma
    parseClosebrack
    c <- many parseStmt
    parseFimprocedimento
    return $ NOVOPROC a b c 

parseOper :: ParseArgs OPER
parseOper = do
    parseOperador
    a <- parseOp
    parseRecebe
    parseOpenbrack
    b <- sepBy parseParam parseComma
    parseClosebrack
    parseRetorna
    c <- many parseModf
    d <- parseTipo
    e <- many parseStmt
    parseFimoperador
    return $ NOVOOPER a b c d e

parseOp :: ParseArgs OP
parseOp = 
    parseNovoadd <|>
    parseNovosub <|>
    parseNovomult <|>
    parseNovodiv <|>
    parseNovogeq <|>
    parseNovoleq <|>
    parseNovodiff <|>
    parseNovoequal <|>
    parseNovogreat <|>
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
    a <- many parseModf
    b <- parseTipo
    c <- parseVar
    return $ NOVOPARAM a b c

parseMain :: ParseArgs MAIN
parseMain = do
    parsePrincipal
    a <- many parseStmt
    parseFimprincipal
    eof
    return $ Main a 

parseStmt :: ParseArgs STMT
parseStmt = 
    parseNovobloco <|>
    parseNovose <|>
    parseNovoenquanto <|>
    parseNovosaia <|>
    parseNovocontinue <|>
    parseNovodelete <|>
    parseNovoescreva <|>
    parseNovoleia <|>
    (try parseNovoretorneproc) <|>
    parseNovoretornefunc <|>
    (try parseNovodec) <|>
    (try parseNovoinc) <|>
    (try parseNovodecr) <|>
    (try parseNovoatrib) <|>
    parseNovochamada

parseNovodec :: ParseArgs STMT
parseNovodec = do
    a <- parseDec
    parseEndcommand
    return $ NOVODEC a 

parseNovoatrib :: ParseArgs STMT
parseNovoatrib = do
    a <- parseCriavalorexpr <|> parseCriavar
    parseAttrib
    b <- parseExpr
    parseEndcommand
    return $ NOVOATRIBSTMT a b

parseNovoinc :: ParseArgs STMT
parseNovoinc = do
    a <- parseInc
    parseEndcommand
    return $ NOVOINC a 

parseNovodecr :: ParseArgs STMT
parseNovodecr = do
    a <- parseDecr
    parseEndcommand
    return $ NOVODECR a 

parseNovochamada :: ParseArgs STMT
parseNovochamada = do
    a <- parseChamada
    parseEndcommand
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
    parseEndcommand
    return $ NOVORETORNEFUNC a 

parseNovoretorneproc :: ParseArgs STMT
parseNovoretorneproc = do
    a <- parseRetorneproc
    parseEndcommand
    return $ NOVORETORNEPROC a 

parseNovosaia :: ParseArgs STMT
parseNovosaia = do
    a <- parseNodesaia
    parseEndcommand
    return $ NOVOSAIA a 

parseNovocontinue :: ParseArgs STMT
parseNovocontinue = do
    a <- parseNodecontinue
    parseEndcommand
    return $ NOVOCONTINUE a 

parseNovodelete :: ParseArgs STMT
parseNovodelete = do
    a <- parseNodedelete
    parseEndcommand
    return $ NOVODELETE a 

parseNovobloco :: ParseArgs STMT
parseNovobloco = do
    a <- parseNodebloco
    return $ NOVOBLOCO a 

parseRetornefunc :: ParseArgs RETORNEFUNC
parseRetornefunc = do
    parseRetorne
    a <- parseExpr
    return $ CRIARETORNEF a 

parseRetorneproc :: ParseArgs RETORNEPROC
parseRetorneproc = do
    parseRetorne
    return $ CRIARETORNEP

parseAtrib :: ParseArgs ATRIB
parseAtrib = do
    a <- parseVar
    parseAttrib
    b <- parseExpr
    return $ CRIAATRIB a b 

parseInc :: ParseArgs INC
parseInc = do
    a <- parseVar
    parseAdd
    parseAdd
    return $ CRIAINC a 

parseDecr :: ParseArgs DECR
parseDecr = do
    a <- parseVar
    parseSub
    parseSub
    return $ CRIADECR a 

parseChamada :: ParseArgs CHAMADA
parseChamada = do
    a <- parseId
    parseOpenbrack
    b <- sepBy parseExpr parseComma
    parseClosebrack
    return $ CRIACHAMADA a b 

parseNovoescreva :: ParseArgs STMT
parseNovoescreva = do
    a <- parseNodeescreva
    parseEndcommand
    return $ NOVOESCREVA a 

parseNovoleia :: ParseArgs STMT
parseNovoleia = do
    a <- parseNodeleia
    parseEndcommand
    return $ NOVOLEIA a

parseNodeleia :: ParseArgs NodeLEIA
parseNodeleia = do
    parseLeia
    parseOpenbrack
    a <- sepBy1 parseVar parseComma
    parseClosebrack
    return $ CRIALEIA a 

parseNodeescreva :: ParseArgs NodeESCREVA
parseNodeescreva = do
    parseEscreva
    parseOpenbrack
    a <- parseExpr
    parseClosebrack
    return $ CRIAESCREVA a 

parseNodebloco :: ParseArgs NodeBLOCO
parseNodebloco = do
    parseBloco
    a <- many parseStmt
    parseFimbloco
    return $ CRIABLOCO a 

parseVar :: ParseArgs VAR
parseVar = do
    a <- sepBy1 parseSinglevar parseDot
    return $ Var a

parseSinglevar :: ParseArgs SingleVAR
parseSinglevar = do
    a <- parseId
    b <- parseOptionalsqbrack
    return $ SingleVar a b

parseOptionalsqbrack :: ParseArgs OptionalSQBRACK
parseOptionalsqbrack =
    parseUsesqbrack <|>
    parseEmptysqbrack

parseUsesqbrack :: ParseArgs OptionalSQBRACK
parseUsesqbrack = do
    parseOpensqbrack
    a <- sepBy parseExpr parseComma
    parseClosesqbrack
    return $ OptionalSQBrack a

parseEmptysqbrack :: ParseArgs OptionalSQBRACK
parseEmptysqbrack = do
    return $ OptionalSQBrack []

parseNodese :: ParseArgs NodeSE
parseNodese = do
    parseSe
    a <- parseExpr
    parseEntao
    b <- many parseStmt
    c <- parseOptionalsenao
    parseFimse
    return $ CRIASE a b c 

parseOptionalsenao :: ParseArgs OptionalSENAO
parseOptionalsenao =
    parseUsesenao <|>
    parseEmptysenao

parseUsesenao :: ParseArgs OptionalSENAO
parseUsesenao = do
    parseSenao
    a <- many parseStmt
    return $ OptionalSenao a

parseEmptysenao :: ParseArgs OptionalSENAO
parseEmptysenao = do
    return $ OptionalSenao []

parseNodeenquanto :: ParseArgs NodeENQUANTO
parseNodeenquanto = do
    parseEnquanto
    a <- parseExpr
    parseExecute
    b <- many parseStmt
    parseFimenquanto
    return $ CRIAENQUANTO a b 

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
    parseDelete
    a <- parseVar
    return $ CRIADELETE a

parseExpr :: ParseArgs EXPR
parseExpr = parseCriaou

parseFim :: EXPR -> ParseArgs EXPR
parseFim a = do return a

parseCriaou :: ParseArgs EXPR
parseCriaou = do
    a <- parseCriae
    b <- parseTentaou a
    return b

parseTentaou :: EXPR -> ParseArgs EXPR
parseTentaou a = (parseContinuaou a) <|> (parseFim a)

parseContinuaou :: EXPR -> ParseArgs EXPR
parseContinuaou a = do
    op <- parseOu <|> parseSlowou
    b <- parseCriaou
    if isOu op then
        return $ CRIAOU a b 
    else
        return $ CRIASLOWOU a b
    where
        isOu (OU _) = True
        isOu _ = False

parseCriae :: ParseArgs EXPR
parseCriae = do
    a <- parseComp
    b <- parseTentae a
    return b

parseTentae :: EXPR -> ParseArgs EXPR
parseTentae a = (parseContinuae a) <|> (parseFim a)

parseContinuae :: EXPR -> ParseArgs EXPR
parseContinuae a = do
    op <- parseE <|> parseSlowe
    b <- parseCriae
    if isE op then
        return $ CRIAE a b 
    else
        return $ CRIASLOWE a b
    where
        isE (E _) = True
        isE _ = False

parseComp :: ParseArgs EXPR
parseComp = do
    a <- parseCriaadd
    b <- parseTentacomp a
    return b

parseTentacomp :: EXPR -> ParseArgs EXPR
parseTentacomp a = (parseContinuacomp a) <|> (parseFim a)

parseContinuacomp :: EXPR -> ParseArgs EXPR
parseContinuacomp a = do
    op <- parseLess <|> parseLeq <|> parseEqual <|> parseGeq <|> parseGreat <|> parseDiff
    b <- parseCriaadd
    
    if isLess op then
        return $ CRIALESS a b
    
    else if isLeq op then
        return $ CRIALEQ a b
    
    else if isEqual op then
        return $ CRIAEQUAL a b
    
    else if isGeq op then
        return $ CRIAGEQ a b
    
    else if isGreat op then
        return $ CRIAGREAT a b
    
    else
        return $ CRIADIFF a b
    
    where
        isLess (Less _) = True
        isLess _ = False
        isLeq (Leq _) = True
        isLeq _ = False
        isEqual (Equal _) = True
        isEqual _ = False
        isGeq (Geq _) = True
        isGeq _ = False
        isGreat (Great _) = True
        isGreat _ = False

parseCriaadd :: ParseArgs EXPR
parseCriaadd = do
    a <- parseCriamult
    b <- parseTentaadd a
    return b

parseTentaadd :: EXPR -> ParseArgs EXPR
parseTentaadd a = (parseContinuaadd a) <|> (parseFim a)

parseContinuaadd :: EXPR -> ParseArgs EXPR
parseContinuaadd a = do
    op <- parseAdd <|> parseSub
    b <- parseCriaadd
    if isAdd op then
        return $ CRIAADD a b 
    else
        return $ CRIASUB a b
    where
        isAdd (Add _) = True
        isAdd _ = False

parseCriamult :: ParseArgs EXPR
parseCriamult = do
    a <- parseAtomico
    b <- parseTentamult a
    return b

parseTentamult :: EXPR -> ParseArgs EXPR
parseTentamult a = (parseContinuamult a) <|> (parseFim a)

parseContinuamult :: EXPR -> ParseArgs EXPR
parseContinuamult a = do
    op <- parseMult <|> parseDiv <|> parseMod
    b <- parseCriamult
    if isMult op then
        return $ CRIAMULT a b 
    else if isDiv op then
        return $ CRIADIV a b
    else
        return $ CRIAMOD a b
    where
        isMult (Mult _) = True
        isMult _ = False
        isDiv (Div _) = True
        isDiv _ = False

parseAtomico :: ParseArgs EXPR
parseAtomico = 
    parseCrianeg <|> 
    parseCrianot <|> 
    parseCriatexto <|> 
    parseCriacaractere <|>
    parseCriaint <|> 
    parseCrialogico <|> 
    parseCriareal <|> 
    parseCrianovo <|> 
    parseCriavalorexpr <|> 
    parseCriaref <|>
    (try parseCriachamadafunc) <|> 
    parseCriavar <|> 
    (try parseCriaconversao) <|>
    parseCriaparenteses

parseCrianeg :: ParseArgs EXPR
parseCrianeg = do
    parseSub
    a <- parseExpr
    return $ CRIANEG a 

parseCrianot :: ParseArgs EXPR
parseCrianot = do
    parseNot
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
    parseNovo
    a <- many parseModf
    b <- parseTipo
    c <- parseOptionalsqbrack
    return $ CRIANOVO a b c

parseCriavalorexpr :: ParseArgs EXPR
parseCriavalorexpr = do
    a <- parseCriavalor
    return $ CRIAVALOREXPR a

parseCriavalor :: ParseArgs VAL
parseCriavalor = (try parseCriaultval) <|> parseCriaseqval
    
parseCriaultval :: ParseArgs VAL
parseCriaultval = do
    parseValor
    parseOpenbrack
    a <- parseVar
    parseClosebrack
    return $ CRIAULTVAL a

parseCriaseqval :: ParseArgs VAL
parseCriaseqval = do
    parseValor
    parseOpenbrack
    a <- parseCriavalor
    parseClosebrack
    return $ CRIASEQVAL a

parseCriaref :: ParseArgs EXPR
parseCriaref = do
    parseReferencia
    parseOpenbrack
    a <- parseVar
    parseClosebrack
    return $ CRIAREFERENCIA a

parseCriaparenteses :: ParseArgs EXPR
parseCriaparenteses = do
    parseOpenbrack
    a <- parseExpr
    parseClosebrack
    return $ CRIAPARENTESES a

parseCriaconversao :: ParseArgs EXPR
parseCriaconversao = do
    parseOpenbrack
    a <- many parseModf
    b <- parseTipo
    parseClosebrack
    c <- parseExpr
    return $ CRIACONVERSAO a b c
