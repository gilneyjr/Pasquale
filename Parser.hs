module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Combinator
import Data.Functor.Identity
import Debug.Trace
import Control.Monad
import Arvore
import ParserTokens
import Lexico

parsePasquale :: String -> PROGRAMA
parsePasquale input =
    getRight (runParser parsePrograma () "" (getTokens input))
    where
        getRight (Right x) = x
        getRight (Left x) = error $ mostra $ getErros (errorMessages x) ["",""]
        mostra [a,""] = ("\n"++a)
        mostra [a,b] = ("\n"++a++"\n"++b)

getErros :: [Message] -> [String] -> [String]
getErros [] x = x
getErros (a:b) l = 
    case a of
        SysUnExpect a -> getErros b (add0 l a)
        UnExpect a -> getErros b (add0 l a)
        Expect a -> getErros b (add1 l a)
        otherwise -> getErros b l
    where
        add0 ["",b] "" = ["Não esperado: Fim de arquivo (EOF)",b]
        add0 ["",b] a@(_:_) = ["Não esperado: " ++ a,b]
        add0 [a@(_:_),b] _ = [a,b]
        add1 [a,""] b = [a,b]
        add1 [a,b@(_:_)] _ = [a,b]

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
    a <- endBy parseDec (parseEndcommand <?> "Esperado: ';'")
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
    a <- many parsePont
    b <- parseTipo
    c <- sepBy1 (parseVar_ <?> "Esperado: identificador da variável a ser declarada") parseComma
    return $ NOVADEC a b c 

parsePont :: ParseArgs PONT
parsePont = do
    a <- parsePonteiro
    return $ NOVOPONT a 

parseVar_ :: ParseArgs VAR_
parseVar_ = do
    a <- parseSinglevar
    (parseVar_com a) <|> (return (VAR_SEM a))

parseVar_sem :: ParseArgs VAR_
parseVar_sem = do
    a <- parseSinglevar
    return $ VAR_SEM a 

parseVar_com :: SingleVAR -> ParseArgs VAR_
parseVar_com a = do
    b <- parseAtrib a
    return $ VAR_COM b

parseEstr :: ParseArgs ESTR
parseEstr = do
    parseEstrutura
    a <- parseTipo <?> "Esperado: Tipo a ser definido"
    b <- endBy parseDecEstr (parseEndcommand <?> "Esperado: ';'")
    parseFimestrutura <?> "Esperado: FIMESTRUTURA"
    return $ NOVOESTR a b

parseDecEstr :: ParseArgs DEC
parseDecEstr = do
    a <- many parsePont
    b <- parseTipo
    c <- sepBy1 (parseVar_sem <?> "Esperado: identificador da variável a ser declarada") parseComma
    return $ NOVADEC a b c 

parseFunc :: ParseArgs FUNC
parseFunc = do
    parseFuncao
    a <- parseId <?> "Esperado: Identificador da função"
    parseRecebe <?> "Esperado: RECEBE"
    parseOpenbrack <?> "Esperado: '('"
    b <- sepBy parseParam parseComma
    parseClosebrack <?> "Esperado: ')'"
    parseRetorna <?> "Esperado: RETORNA"
    c <- many parsePont
    d <- parseTipo <?> "Esperado: Tipo a ser retornado"
    e <- many parseStmt
    parseFimfuncao <?> "Esperado: FIMFUNCAO"
    return $ NOVOFUNC a b c d e

parseProc :: ParseArgs PROC
parseProc = do
    parseProcedimento
    a <- parseId <?> "Esperado: Identificador do procedimento"
    parseRecebe <?> "Esperado: RECEBE"
    parseOpenbrack <?> "Esperado: '('"
    b <- sepBy parseParam parseComma
    parseClosebrack <?> "Esperado: ')'"
    c <- many parseStmt
    parseFimprocedimento <?> "Esperado: FIMPROCEDIMENTO"
    return $ NOVOPROC a b c 

parseOper :: ParseArgs OPER
parseOper = do
    parseOperador
    a <- parseOp <?> "Esperado: Identificador do operador a ser sobrecarregado"
    parseRecebe <?> "Esperado: RECEBE"
    parseOpenbrack <?> "Esperado: '('"
    b <- sepBy parseParam parseComma
    parseClosebrack <?> "Esperado: ')'"
    parseRetorna <?> "Esperado: RETORNA"
    c <- many parsePont
    d <- parseTipo <?> "Esperado: Tipo a ser retornado"
    e <- many parseStmt
    parseFimoperador <?> "Esperado: FIMOPERADOR"
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
    parseNovoless <|>
    parseNovonot

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
    
parseNovonot :: ParseArgs OP
parseNovonot = do
    a <- parseNot
    return $ NOVONot a 

parseParam :: ParseArgs PARAM
parseParam = do
    a <- many parsePont
    b <- parseTipo
    c <- parseId <?> "Esperado: Identificador do parâmetro"
    return $ NOVOPARAM a b (SingleVar c (OptionalSQBrack []))

parseMain :: ParseArgs MAIN
parseMain = do
    parsePrincipal <?> "Esperado: PRINCIPAL"
    a <- many parseStmt
    parseFimprincipal <?> "Esperado: FIMPRINCIPAL"
    eof <?> "Esperado: Fim de arquivo (EOF)"
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
    parseNovoretorne <|>
    parseNovodec <|>
    parseStmtscomID
    
parseStmtscomID :: ParseArgs STMT
parseStmtscomID = do
    a <- parseCriavalorexpr <|> parseCriavar
    case a of
        CRIAVAR (Var [SingleVar t (OptionalSQBrack [])]) ->
            ( (parseNovoinc a) <|>
            (parseNovodecr a) <|>
            (parseNovoatrib a) <|>
            (parseNovochamada t) ) <?> "Sugestão: \"++\", \"--\", \"(\", \":=\""
        otherwise ->
            ( (parseNovoinc a) <|>
            (parseNovodecr a) <|>
            (parseNovoatrib a) ) <?> "Sugestão: \"++\", \"--\", \":=\""

parseNovodec :: ParseArgs STMT
parseNovodec = do
    a <- parseDec
    parseEndcommand <?> "Esperado: ';'"
    return $ NOVODEC a 

parseNovoatrib :: EXPR -> ParseArgs STMT
parseNovoatrib a = do
    b <- parseAttrib
    c <- parseExpr <?> "Esperado: Expressão a ser atribuída"
    parseEndcommand <?> "Esperado: ';'"
    return $ NOVOATRIBSTMT a b c

parseNovoinc :: EXPR -> ParseArgs STMT
parseNovoinc a = do
    b <- parseInc a
    parseEndcommand <?> "Esperado: ';'"
    return $ NOVOINC b

parseNovodecr :: EXPR -> ParseArgs STMT
parseNovodecr a = do
    b <- parseDecr a
    parseEndcommand <?> "Esperado: ';'"
    return $ NOVODECR b

parseNovochamada :: Token -> ParseArgs STMT
parseNovochamada a = do
    b <- parseChamada a
    parseEndcommand <?> "Esperado: ';'"
    return $ NOVOCHAMADA b

parseNovose :: ParseArgs STMT
parseNovose = do
    a <- parseNodese
    return $ NOVOSE a 

parseNovoenquanto :: ParseArgs STMT
parseNovoenquanto = do
    a <- parseNodeenquanto
    return $ NOVOENQUANTO a 

parseNovoretorne :: ParseArgs STMT
parseNovoretorne = do
    a <- parseRetorne
    (parseNovoretornefunc a) <|> (parseNovoretorneproc a)
    
parseNovoretornefunc :: Token -> ParseArgs STMT
parseNovoretornefunc a = do
    b <- parseRetornefunc a
    parseEndcommand <?> "Esperado: ';'"
    return $ NOVORETORNEFUNC b

parseNovoretorneproc :: Token -> ParseArgs STMT
parseNovoretorneproc a = do
    b <- parseRetorneproc a
    parseEndcommand <?> "Esperado: ';'"
    return $ NOVORETORNEPROC b

parseRetornefunc :: Token -> ParseArgs RETORNEFUNC
parseRetornefunc a = do
    b <- parseExpr
    return $ CRIARETORNEF a b

parseRetorneproc :: Token -> ParseArgs RETORNEPROC
parseRetorneproc a = do
    return $ CRIARETORNEP a

parseNovosaia :: ParseArgs STMT
parseNovosaia = do
    a <- parseNodesaia
    parseEndcommand <?> "Esperado: ';'"
    return $ NOVOSAIA a 

parseNovocontinue :: ParseArgs STMT
parseNovocontinue = do
    a <- parseNodecontinue
    parseEndcommand <?> "Esperado: ';'"
    return $ NOVOCONTINUE a 

parseNovodelete :: ParseArgs STMT
parseNovodelete = do
    a <- parseNodedelete
    parseEndcommand <?> "Esperado: ';'"
    return $ NOVODELETE a 

parseNovobloco :: ParseArgs STMT
parseNovobloco = do
    a <- parseNodebloco
    return $ NOVOBLOCO a 

parseAtrib :: SingleVAR -> ParseArgs ATRIB
parseAtrib a = do
    parseAttrib
    b <- parseExpr <?> "Esperado: Expressão a ser atribuída"
    return $ CRIAATRIB a b 

parseInc :: EXPR -> ParseArgs INC
parseInc a = do
    parseAdd
    parseAdd <?> "Esperado: '+'"
    return $ CRIAINC a 

parseDecr :: EXPR -> ParseArgs DECR
parseDecr a = do
    parseSub
    parseSub <?> "Esperado: '-'"
    return $ CRIADECR a 

parseChamada :: Token -> ParseArgs CHAMADA
parseChamada a = do
    parseOpenbrack
    b <- sepBy parseExpr parseComma
    parseClosebrack <?> "Esperado: ')'"
    return $ CRIACHAMADA a b 

parseNovoescreva :: ParseArgs STMT
parseNovoescreva = do
    a <- parseNodeescreva
    parseEndcommand <?> "Esperado: ';'"
    return $ NOVOESCREVA a 

parseNovoleia :: ParseArgs STMT
parseNovoleia = do
    a <- parseNodeleia
    parseEndcommand <?> "Esperado: ';'"
    return $ NOVOLEIA a

parseNodeleia :: ParseArgs NodeLEIA
parseNodeleia = do
    a <- parseLeia
    parseOpenbrack <?> "Esperado: '('"
    b <- sepBy1 ((parseCriavalorexpr <|> parseCriavar) <?> "Esperado: variável a ser lida") parseComma
    parseClosebrack <?> "Esperado: ')'"
    return $ CRIALEIA a b

parseNodeescreva :: ParseArgs NodeESCREVA
parseNodeescreva = do
    a <- parseEscreva
    parseOpenbrack <?> "Esperado: '('"
    b <- parseExpr
    parseClosebrack <?> "Esperado: ')'"
    return $ CRIAESCREVA a b

parseNodebloco :: ParseArgs NodeBLOCO
parseNodebloco = do
    parseBloco
    a <- many parseStmt
    parseFimbloco <?> "Esperado: FIMBLOCO"
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
    a <- sepBy1 (parseExpr <?> "Esperado: Índice do elemento do arranjo a ser acessado") parseComma
    parseClosesqbrack <?> "Esperado: ']'"
    return $ OptionalSQBrack a

parseEmptysqbrack :: ParseArgs OptionalSQBRACK
parseEmptysqbrack = do
    return $ OptionalSQBrack []

parseNodese :: ParseArgs NodeSE
parseNodese = do
    token <- parseSe
    a <- parseExpr <?> "Esperado: Condição do SE"
    parseEntao <?> "Esperado: ENTAO"
    b <- many parseStmt
    c <- parseOptionalsenao
    parseFimse <?> "Esperado: FIMSE"
    return $ CRIASE token a b c 

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
    a <- parseEnquanto
    b <- parseExpr <?> "Esperado: Condição do ENQUANTO"
    parseExecute <?> "Esperado: EXECUTE"
    c <- many parseStmt
    parseFimenquanto <?> "Esperado: FIMENQUANTO"
    return $ CRIAENQUANTO a b c

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
    b <- parseExpr <?> "Esperado: Ponteiro a ser desalocado"
    return $ CRIADELETE a b

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
    if isOu op then do
        b <- parseCriae <?> "Esperado: Valor direito a ser operado com o comando \"OU\""
        c <- parseTentaou $ CRIAOU a op b 
        return c
    else do
        b <- parseCriae <?> "Esperado: Valor direito a ser operado com o comando \"~OU\""
        c <- parseTentaou $ CRIASLOWOU a op b
        return c
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
    if isE op then do
        b <- parseComp <?> "Esperado: Valor direito a ser operado com o comando \"E\""
        c <- parseTentae $ CRIAE a op b
        return c 
    else do
        b <- parseComp <?> "Esperado: Valor direito a ser operado com o comando \"~E\""
        c <- parseTentae $ CRIASLOWE a op b
        return c
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
    
    if isLess op then do
        b <- parseCriaadd <?> "Esperado: Valor direito a ser operado com o operador \"<\""
        return $ CRIALESS a op b
    
    else if isLeq op then do
        b <- parseCriaadd <?> "Esperado: Valor direito a ser operado com o operador \"<=\""
        return $ CRIALEQ a op b
    
    else if isEqual op then do
        b <- parseCriaadd <?> "Esperado: Valor direito a ser operado com o operador \"=\""
        return $ CRIAEQUAL a op b
    
    else if isGeq op then do
        b <- parseCriaadd <?> "Esperado: Valor direito a ser operado com o operador \">=\""
        return $ CRIAGEQ a op b
    
    else if isGreat op then do
        b <- parseCriaadd <?> "Esperado: Valor direito a ser operado com o operador \">\""
        return $ CRIAGREAT a op b
    
    else do
        b <- parseCriaadd <?> "Esperado: Valor direito a ser operado com o operador \"/=\""
        return $ CRIADIFF a op b
    
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
    if isAdd op then do
        b <- parseCriamult <?> "Esperado: Valor direito a ser operado com o operador \"+\""
        c <- parseTentaadd $ CRIAADD a op b
        return c
    else do
        b <- parseCriamult <?> "Esperado: Valor direito a ser operado com o operador \"-\""
        c <- parseTentaadd $ CRIASUB a op b
        return c
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
    if isMult op then do
        b <- parseAtomico <?> "Esperado: Valor direito a ser operador com o operador \"*\""
        c <- parseTentamult $ CRIAMULT a op b 
        return c
    else if isDiv op then do
        b <- parseAtomico <?> "Esperado: Valor direito a ser operador com o operador \"/\""
        c <- parseTentamult $ CRIADIV a op b
        return c
    else do
        b <- parseAtomico <?> "Esperado: Valor direito a ser operador com o comando \"MOD\""
        c <- parseTentamult $ CRIAMOD a op b
        return c
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
    parseCrianulo <|> 
    parseCrianovo <|> 
    parseCriavalorexpr <|> 
    parseCriaExprID <|>
    parseIniciabrack

parseCrianeg :: ParseArgs EXPR
parseCrianeg = do
    op <- parseSub
    a <- parseAtomico <?> "Esperado: Expressão a ser operada pelo operador \"-\""
    return $ CRIANEG op a 

parseCrianot :: ParseArgs EXPR
parseCrianot = do
    op <- parseNot
    a <- parseAtomico <?> "Esperado: Expressão a ser operada pelo operador \"!\""
    return $ CRIANOT op a 

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

parseCrianulo :: ParseArgs EXPR
parseCrianulo = do
    a <- parseNulo
    return $ CRIANULO a 

parseCriavar :: ParseArgs EXPR
parseCriavar = do
    a <- parseVar
    return $ CRIAVAR a 

parseCrianovo :: ParseArgs EXPR
parseCrianovo = do
    parseNovo
    a <- many parsePont
    b <- parseTipo <?> "Esperado: Tipo a ser alocado"
    return $ CRIANOVO a b

parseCriavalorexpr :: ParseArgs EXPR
parseCriavalorexpr = do
    a <- parseValor
    parseOpenbrack <?> "Esperado: '('"
    b <- parseExpr <?> "Esperado: Expressão interna de VALOR"
    parseClosebrack <?> "Esperado: ')'"
    return $ CRIAVALOREXPR a b

parseCriaExprID :: ParseArgs EXPR
parseCriaExprID = do
    a <- parseCriavar
    case a of
        CRIAVAR (Var [SingleVar id (OptionalSQBrack []) ]) -> ((parseCriachamadafunc id) <|> (return a))
        otherwise -> return a

parseCriachamadafunc :: Token -> ParseArgs EXPR
parseCriachamadafunc a = do
    b <- parseChamada a
    return $ CRIACHAMADAFUNC b

parseIniciabrack :: ParseArgs EXPR
parseIniciabrack = do
    parseOpenbrack
    (parseCriaconversao <|> parseCriaparenteses) <?> "Esperado: Tipo a ser convertido ou expressão entre parênteses"

parseCriaparenteses :: ParseArgs EXPR
parseCriaparenteses = do
    a <- parseExpr
    parseClosebrack <?> "Esperado: ')'"
    return $ CRIAPARENTESES a

parseCriaconversao :: ParseArgs EXPR
parseCriaconversao = do
    a <- parseTipo
    parseClosebrack <?> "Esperado: ')'"
    b <- parseAtomico <?> "Esperado: Valor a ser convertido"
    return $ CRIACONVERSAO a b 
    
    

