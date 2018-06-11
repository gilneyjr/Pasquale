import Text.ParserCombinators.Parsec
import qualified Parser
import qualified Lexico
import qualified Interpretador

import System.Environment
import System.IO
import Interpretador
import Arvore

main :: IO() 
main = do
    args <- getArgs
    fileText <- openFile (args !! 0) ReadMode
    s <- hGetContents fileText
    --Lexico.getTokens s
    let s' = Parser.parsePasquale s
    case s' of
        Right a -> executaPrograma a
        Left erro -> print erro

