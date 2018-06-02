module Parser where

import Text.Parsec
import Arvore
import ParserTokens
import Lexico

parsePasquale :: String -> Either ParseError PROGRAMA
parsePasquale input = runParser parsePrograma () "" (getTokens input)

parsePrograma :: ParseArgs PROGRAMA
parsePrograma = do
    a <- parseMain
    return $ CRIAPROG a

parseMain :: ParseArgs MAIN
parseMain = do
    parsePrincipal
    a <- many parseStmt
    parseFimprincipal
    return $ Main a

parseStmt :: ParseArgs STMT
parseStmt = do
    a <- parseDec
    parseEndcommand
    return $ NOVODEC a

parseDec :: ParseArgs DEC
parseDec = do
    a <- parseTipo
    b <- parseVar_
    return $ NOVADEC a b

parseVar_ :: ParseArgs VAR_
parseVar_ = 
    (try parseVar_com) <|>
    parseVar_sem

parseVar_sem :: ParseArgs VAR_
parseVar_sem = do
    a <- parseId
    return $ VAR_SEM a 

parseVar_com :: ParseArgs VAR_
parseVar_com = do
    a <- parseAtrib
    return $ VAR_COM a

parseAtrib :: ParseArgs ATRIB
parseAtrib = do
    a <- parseId
    parseAttrib
    b <- parseExpr
    return $ CRIAATRIB a b 

-- Expressões
parseExpr :: ParseArgs EXPR
parseExpr = parseCriaInteiro <|> parseCriaparenteses

parseCriaInteiro :: ParseArgs EXPR
parseCriaInteiro = do 
    a <- parseInteiro
    return $ CRIAINT a

parseCriaparenteses :: ParseArgs EXPR
parseCriaparenteses = do
    parseOpenbrack
    a <- parseExpr
    parseClosebrack
    return $ CRIAPARENTESES a








