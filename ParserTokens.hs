module ParserTokens where
import Data.Functor.Identity
import Text.Parsec
import Lexico
import Arvore

parseEstrutura :: ParsecT [Token] u Identity Token
parseEstrutura = tokenPrim show update_pos get_token where
    get_token ( ESTRUTURA x ) = Just ( ESTRUTURA x )
    get_token _ = Nothing

parseFimestrutura :: ParsecT [Token] u Identity Token
parseFimestrutura = tokenPrim show update_pos get_token where
    get_token ( FIMESTRUTURA x ) = Just ( FIMESTRUTURA x )
    get_token _ = Nothing

parseFuncao :: ParsecT [Token] u Identity Token
parseFuncao = tokenPrim show update_pos get_token where
    get_token ( FUNCAO x ) = Just ( FUNCAO x )
    get_token _ = Nothing

parseFimfuncao :: ParsecT [Token] u Identity Token
parseFimfuncao = tokenPrim show update_pos get_token where
    get_token ( FIMFUNCAO x ) = Just ( FIMFUNCAO x )
    get_token _ = Nothing

parseProcedimento :: ParsecT [Token] u Identity Token
parseProcedimento = tokenPrim show update_pos get_token where
    get_token ( PROCEDIMENTO x ) = Just ( PROCEDIMENTO x )
    get_token _ = Nothing

parseFimprocedimento :: ParsecT [Token] u Identity Token
parseFimprocedimento = tokenPrim show update_pos get_token where
    get_token ( FIMPROCEDIMENTO x ) = Just ( FIMPROCEDIMENTO x )
    get_token _ = Nothing

parseOperador :: ParsecT [Token] u Identity Token
parseOperador = tokenPrim show update_pos get_token where
    get_token ( OPERADOR x ) = Just ( OPERADOR x )
    get_token _ = Nothing

parseFimoperador :: ParsecT [Token] u Identity Token
parseFimoperador = tokenPrim show update_pos get_token where
    get_token ( FIMOPERADOR x ) = Just ( FIMOPERADOR x )
    get_token _ = Nothing

parseRecebe :: ParsecT [Token] u Identity Token
parseRecebe = tokenPrim show update_pos get_token where
    get_token ( RECEBE x ) = Just ( RECEBE x )
    get_token _ = Nothing

parseRetorna :: ParsecT [Token] u Identity Token
parseRetorna = tokenPrim show update_pos get_token where
    get_token ( RETORNA x ) = Just ( RETORNA x )
    get_token _ = Nothing

parseRetorne :: ParsecT [Token] u Identity Token
parseRetorne = tokenPrim show update_pos get_token where
    get_token ( RETORNE x ) = Just ( RETORNE x )
    get_token _ = Nothing

parsePrincipal :: ParsecT [Token] u Identity Token
parsePrincipal = tokenPrim show update_pos get_token where
    get_token ( PRINCIPAL x ) = Just ( PRINCIPAL x )
    get_token _ = Nothing

parseFimprincipal :: ParsecT [Token] u Identity Token
parseFimprincipal = tokenPrim show update_pos get_token where
    get_token ( FIMPRINCIPAL x ) = Just ( FIMPRINCIPAL x )
    get_token _ = Nothing

parseBloco :: ParsecT [Token] u Identity Token
parseBloco = tokenPrim show update_pos get_token where
    get_token ( BLOCO x ) = Just ( BLOCO x )
    get_token _ = Nothing

parseFimbloco :: ParsecT [Token] u Identity Token
parseFimbloco = tokenPrim show update_pos get_token where
    get_token ( FIMBLOCO x ) = Just ( FIMBLOCO x )
    get_token _ = Nothing

parseSaia :: ParsecT [Token] u Identity Token
parseSaia = tokenPrim show update_pos get_token where
    get_token ( SAIA x ) = Just ( SAIA x )
    get_token _ = Nothing

parseContinue :: ParsecT [Token] u Identity Token
parseContinue = tokenPrim show update_pos get_token where
    get_token ( CONTINUE x ) = Just ( CONTINUE x )
    get_token _ = Nothing

parseSe :: ParsecT [Token] u Identity Token
parseSe = tokenPrim show update_pos get_token where
    get_token ( SE x ) = Just ( SE x )
    get_token _ = Nothing

parseEntao :: ParsecT [Token] u Identity Token
parseEntao = tokenPrim show update_pos get_token where
    get_token ( ENTAO x ) = Just ( ENTAO x )
    get_token _ = Nothing

parseSenao :: ParsecT [Token] u Identity Token
parseSenao = tokenPrim show update_pos get_token where
    get_token ( SENAO x ) = Just ( SENAO x )
    get_token _ = Nothing

parseFimse :: ParsecT [Token] u Identity Token
parseFimse = tokenPrim show update_pos get_token where
    get_token ( FIMSE x ) = Just ( FIMSE x )
    get_token _ = Nothing

parseEnquanto :: ParsecT [Token] u Identity Token
parseEnquanto = tokenPrim show update_pos get_token where
    get_token ( ENQUANTO x ) = Just ( ENQUANTO x )
    get_token _ = Nothing

parseExecute :: ParsecT [Token] u Identity Token
parseExecute = tokenPrim show update_pos get_token where
    get_token ( EXECUTE x ) = Just ( EXECUTE x )
    get_token _ = Nothing

parseFimenquanto :: ParsecT [Token] u Identity Token
parseFimenquanto = tokenPrim show update_pos get_token where
    get_token ( FIMENQUANTO x ) = Just ( FIMENQUANTO x )
    get_token _ = Nothing

parseDefina :: ParsecT [Token] u Identity Token
parseDefina = tokenPrim show update_pos get_token where
    get_token ( DEFINA x ) = Just ( DEFINA x )
    get_token _ = Nothing

parseSlowou :: ParsecT [Token] u Identity Token
parseSlowou = tokenPrim show update_pos get_token where
    get_token ( SlowOU x ) = Just ( SlowOU x )
    get_token _ = Nothing

parseSlowe :: ParsecT [Token] u Identity Token
parseSlowe = tokenPrim show update_pos get_token where
    get_token ( SlowE x ) = Just ( SlowE x )
    get_token _ = Nothing

parseOu :: ParsecT [Token] u Identity Token
parseOu = tokenPrim show update_pos get_token where
    get_token ( OU x ) = Just ( OU x )
    get_token _ = Nothing

parseE :: ParsecT [Token] u Identity Token
parseE = tokenPrim show update_pos get_token where
    get_token ( E x ) = Just ( E x )
    get_token _ = Nothing

parseLogico :: ParsecT [Token] u Identity Token
parseLogico = tokenPrim show update_pos get_token where
    get_token ( LOGICO x y ) = Just ( LOGICO x y )
    get_token _ = Nothing

parsePonteiro :: ParsecT [Token] u Identity Token
parsePonteiro = tokenPrim show update_pos get_token where
    get_token ( PONTEIRO x ) = Just ( PONTEIRO x )
    get_token _ = Nothing

parseNovo :: ParsecT [Token] u Identity Token
parseNovo = tokenPrim show update_pos get_token where
    get_token ( NOVO x ) = Just ( NOVO x )
    get_token _ = Nothing

parseDelete :: ParsecT [Token] u Identity Token
parseDelete = tokenPrim show update_pos get_token where
    get_token ( DELETE x ) = Just ( DELETE x )
    get_token _ = Nothing

parseLeia :: ParsecT [Token] u Identity Token
parseLeia = tokenPrim show update_pos get_token where
    get_token ( LEIA x ) = Just ( LEIA x )
    get_token _ = Nothing

parseEscreva :: ParsecT [Token] u Identity Token
parseEscreva = tokenPrim show update_pos get_token where
    get_token ( ESCREVA x ) = Just ( ESCREVA x )
    get_token _ = Nothing

parseValor :: ParsecT [Token] u Identity Token
parseValor = tokenPrim show update_pos get_token where
    get_token ( VALOR x ) = Just ( VALOR x )
    get_token _ = Nothing

parseAttrib :: ParsecT [Token] u Identity Token
parseAttrib = tokenPrim show update_pos get_token where
    get_token ( Attrib x ) = Just ( Attrib x )
    get_token _ = Nothing

parseGeq :: ParsecT [Token] u Identity Token
parseGeq = tokenPrim show update_pos get_token where
    get_token ( Geq x ) = Just ( Geq x )
    get_token _ = Nothing

parseLeq :: ParsecT [Token] u Identity Token
parseLeq = tokenPrim show update_pos get_token where
    get_token ( Leq x ) = Just ( Leq x )
    get_token _ = Nothing

parseDiff :: ParsecT [Token] u Identity Token
parseDiff = tokenPrim show update_pos get_token where
    get_token ( Diff x ) = Just ( Diff x )
    get_token _ = Nothing

parseEqual :: ParsecT [Token] u Identity Token
parseEqual = tokenPrim show update_pos get_token where
    get_token ( Equal x ) = Just ( Equal x )
    get_token _ = Nothing

parseGreat :: ParsecT [Token] u Identity Token
parseGreat = tokenPrim show update_pos get_token where
    get_token ( Great x ) = Just ( Great x )
    get_token _ = Nothing

parseLess :: ParsecT [Token] u Identity Token
parseLess = tokenPrim show update_pos get_token where
    get_token ( Less x ) = Just ( Less x )
    get_token _ = Nothing

parseAdd :: ParsecT [Token] u Identity Token
parseAdd = tokenPrim show update_pos get_token where
    get_token ( Add x ) = Just ( Add x )
    get_token _ = Nothing

parseSub :: ParsecT [Token] u Identity Token
parseSub = tokenPrim show update_pos get_token where
    get_token ( Sub x ) = Just ( Sub x )
    get_token _ = Nothing

parseMult :: ParsecT [Token] u Identity Token
parseMult = tokenPrim show update_pos get_token where
    get_token ( Mult x ) = Just ( Mult x )
    get_token _ = Nothing

parseDiv :: ParsecT [Token] u Identity Token
parseDiv = tokenPrim show update_pos get_token where
    get_token ( Div x ) = Just ( Div x )
    get_token _ = Nothing

parseMod :: ParsecT [Token] u Identity Token
parseMod = tokenPrim show update_pos get_token where
    get_token ( MOD x ) = Just ( MOD x )
    get_token _ = Nothing

parseNot :: ParsecT [Token] u Identity Token
parseNot = tokenPrim show update_pos get_token where
    get_token ( Not x ) = Just ( Not x )
    get_token _ = Nothing

parseOpenbrack :: ParsecT [Token] u Identity Token
parseOpenbrack = tokenPrim show update_pos get_token where
    get_token ( OpenBrack x ) = Just ( OpenBrack x )
    get_token _ = Nothing

parseClosebrack :: ParsecT [Token] u Identity Token
parseClosebrack = tokenPrim show update_pos get_token where
    get_token ( CloseBrack x ) = Just ( CloseBrack x )
    get_token _ = Nothing

parseOpensqbrack :: ParsecT [Token] u Identity Token
parseOpensqbrack = tokenPrim show update_pos get_token where
    get_token ( OpenSqBrack x ) = Just ( OpenSqBrack x )
    get_token _ = Nothing

parseClosesqbrack :: ParsecT [Token] u Identity Token
parseClosesqbrack = tokenPrim show update_pos get_token where
    get_token ( CloseSqBrack x ) = Just ( CloseSqBrack x )
    get_token _ = Nothing

parseComma :: ParsecT [Token] u Identity Token
parseComma = tokenPrim show update_pos get_token where
    get_token ( Comma x ) = Just ( Comma x )
    get_token _ = Nothing

parseDot :: ParsecT [Token] u Identity Token
parseDot = tokenPrim show update_pos get_token where
    get_token ( Dot x ) = Just ( Dot x )
    get_token _ = Nothing

parseEndcommand :: ParsecT [Token] u Identity Token
parseEndcommand = tokenPrim show update_pos get_token where
    get_token ( EndCommand x ) = Just ( EndCommand x )
    get_token _ = Nothing

parseReal :: ParsecT [Token] u Identity Token
parseReal = tokenPrim show update_pos get_token where
    get_token ( REAL x y ) = Just ( REAL x y )
    get_token _ = Nothing

parseInteiro :: ParsecT [Token] u Identity Token
parseInteiro = tokenPrim show update_pos get_token where
    get_token ( INTEIRO x y ) = Just ( INTEIRO x y )
    get_token _ = Nothing

parseCaractere :: ParsecT [Token] u Identity Token
parseCaractere = tokenPrim show update_pos get_token where
    get_token ( CARACTERE x y ) = Just ( CARACTERE x y )
    get_token _ = Nothing

parseTexto :: ParsecT [Token] u Identity Token
parseTexto = tokenPrim show update_pos get_token where
    get_token ( TEXTO x y ) = Just ( TEXTO x y )
    get_token _ = Nothing

parseTipo :: ParsecT [Token] u Identity Token
parseTipo = tokenPrim show update_pos get_token where
    get_token ( TIPO x y ) = Just ( TIPO x y )
    get_token _ = Nothing

parseId :: ParsecT [Token] u Identity Token
parseId = tokenPrim show update_pos get_token where
    get_token ( ID x y ) = Just ( ID x y )
    get_token _ = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos
update_pos pos _ []      = pos
