module ParserTokens where

import Control.Monad.Identity
import Text.Parsec
import Lexico
import Arvore

parsePrincipal :: ParsecT [Token] u Identity Token
parsePrincipal = tokenPrim show update_pos get_token where
    get_token ( PRINCIPAL x ) = Just ( PRINCIPAL x )
    get_token _ = Nothing

parseFimprincipal :: ParsecT [Token] u Identity Token
parseFimprincipal = tokenPrim show update_pos get_token where
    get_token ( FIMPRINCIPAL x ) = Just ( FIMPRINCIPAL x )
    get_token _ = Nothing

parseAttrib :: ParsecT [Token] u Identity Token
parseAttrib = tokenPrim show update_pos get_token where
    get_token ( Attrib x ) = Just ( Attrib x )
    get_token _ = Nothing

parseTipo :: ParsecT [Token] u Identity Token
parseTipo = tokenPrim show update_pos get_token where
    get_token ( TIPO x y ) = Just ( TIPO x y )
    get_token _ = Nothing

parseId :: ParsecT [Token] u Identity Token
parseId = tokenPrim show update_pos get_token where
    get_token ( ID x y ) = Just ( ID x y )
    get_token _ = Nothing

parseOpenbrack :: ParsecT [Token] u Identity Token
parseOpenbrack = tokenPrim show update_pos get_token where
    get_token ( OpenBrack x ) = Just ( OpenBrack x )
    get_token _ = Nothing

parseClosebrack :: ParsecT [Token] u Identity Token
parseClosebrack = tokenPrim show update_pos get_token where
    get_token ( CloseBrack x ) = Just ( CloseBrack x )
    get_token _ = Nothing

parseEndcommand :: ParsecT [Token] u Identity Token
parseEndcommand = tokenPrim show update_pos get_token where
    get_token ( EndCommand x ) = Just ( EndCommand x )
    get_token _ = Nothing

parseInteiro :: ParsecT [Token] u Identity Token
parseInteiro = tokenPrim show update_pos get_token where
    get_token ( INTEIRO x y ) = Just ( INTEIRO x y )
    get_token _ = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ _ = pos