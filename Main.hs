import Text.ParserCombinators.Parsec
import qualified Parser
import qualified Interpretador

import System.Environment
import System.IO
import Arvore

main :: IO() 
main = do
    args <- getArgs
    fileText <- openFile (args !! 0) ReadMode
    s <- hGetContents fileText
    Interpretador.executaPrograma $ Parser.parsePasquale s

