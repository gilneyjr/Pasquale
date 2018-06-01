module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Data.Functor.Identity
import Control.Monad
import Arvore
import ParserTokens
import Lexico
import qualified Text.Parsec.Prim as P

parsePasquale :: String -> Either ParseError PROGRAMA
parsePasquale input = runParser parserPrograma () "" (getTokens input)

type ParseArgs = P.ParsecT [Token] () Identity

parserPrograma :: ParseArgs PROGRAMA
parserPrograma = do return $ CRIAPROG parseEstrs

parseEstrs :: ParseArgs ESTRS
parseEstrs = do
    estrs <- estruturatoken
    decs <- estruturatoken
    return (AA estrs decs)

