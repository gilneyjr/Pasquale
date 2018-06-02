import Text.ParserCombinators.Parsec
import qualified Parser

import System.Environment
import System.IO

main :: IO() 
main = do
    args <- getArgs
    fileText <- openFile (args !! 0) ReadMode
    s <- hGetContents fileText
    print $ Parser.parsePasquale s

