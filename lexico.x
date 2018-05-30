{
module Lexico (Token(..), alexScanTokens, token_pos, getTokens) where
import System.IO.Unsafe
import System.IO
}

-- Using the "posn" wrapper
%wrapper "posn"

$digit = 0-9         -- digits
$alpha = [a-zA-Z]    -- alphabetic characters
$loweralpha = [a-z]  -- lowercase alphabetic characters
$upperalpha = [A-Z]  -- uppercase alphabetic characters

-- Regular expressions that define the language tokens.
tokens :-
  $white+                           ;
  "--".*                            ;
  ESTRUTURA                         { \p s -> ESTRUTURA (getPosition p) }
  FIMESTRUTURA                      { \p s -> FIMESTRUTURA (getPosition p) }
  FUNCAO                            { \p s -> FUNCAO (getPosition p) }
  FIMFUNCAO                         { \p s -> FIMFUNCAO (getPosition p) }
  PROCEDIMENTO                      { \p s -> PROCEDIMENTO (getPosition p) }
  FIMPROCEDIMENTO                   { \p s -> FIMPROCEDIMENTO (getPosition p) }
  OPERADOR                          { \p s -> OPERADOR (getPosition p) }
  FIMOPERADOR                       { \p s -> FIMOPERADOR (getPosition p) }
  RECEBE                            { \p s -> RECEBE (getPosition p) }
  RETORNA                           { \p s -> RETORNA (getPosition p) }
  RETORNE                           { \p s -> RETORNE (getPosition p) }
  PRINCIPAL                         { \p s -> PRINCIPAL (getPosition p) }
  FIMPRINCIPAL                      { \p s -> FIMPRINCIPAL (getPosition p) }
  SAIA                              { \p s -> SAIA (getPosition p) }
  CONTINUE                          { \p s -> CONTINUE (getPosition p) }
  SE                                { \p s -> SE (getPosition p) }
  ENTAO                             { \p s -> ENTAO (getPosition p) }
  SENAO                             { \p s -> SENAO (getPosition p) }
  FIMSE                             { \p s -> FIMSE (getPosition p) }
  ENQUANTO                          { \p s -> ENQUANTO (getPosition p) }
  EXECUTE                           { \p s -> EXECUTE (getPosition p) }
  FIMENQUANTO                       { \p s -> FIMENQUANTO (getPosition p) }
  DEFINA                            { \p s -> DEFINA (getPosition p) }
  "~OU"                             { \p s -> SlowOU (getPosition p) }
  "~E"                              { \p s -> SlowE (getPosition p) }
  OU                                { \p s -> OU (getPosition p) }
  E                                 { \p s -> E (getPosition p) }
  VERDADEIRO                        { \p s -> LOGICO (getPosition p) (getBoolValue s) }
  FALSO                             { \p s -> LOGICO (getPosition p) (getBoolValue s) }
  PONT                              { \p s -> PONT (getPosition p) }
  NOVO                              { \p s -> NOVO (getPosition p) }
  DELETE                            { \p s -> DELETE (getPosition p) }
  CONST                             { \p s -> CONST (getPosition p) }
  ESCREVA                           { \p s -> ESCREVA (getPosition p) }
  LEIA                              { \p s -> LEIA (getPosition p) }
  ":="                              { \p s -> Attrib (getPosition p) }
  ">="                              { \p s -> Geq (getPosition p) }
  "<="                              { \p s -> Leq (getPosition p) }
  "/="                              { \p s -> Diff (getPosition p) }
  "="                               { \p s -> Equal (getPosition p) }
  ">"                               { \p s -> Great (getPosition p) }
  "<"                               { \p s -> Less (getPosition p) }
  "+"                               { \p s -> Add (getPosition p) }
  "-"                               { \p s -> Sub (getPosition p) }
  "*"                               { \p s -> Mult (getPosition p) }
  "/"                               { \p s -> Div (getPosition p) }
  "MOD"                             { \p s -> MOD (getPosition p) }
  "!"                               { \p s -> NOT (getPosition p) }
  "("                               { \p s -> OpenBrack (getPosition p) }
  ")"                               { \p s -> CloseBrack (getPosition p) }
  "["                               { \p s -> OpenSqBrack (getPosition p) }
  "]"                               { \p s -> CloseSqBrack (getPosition p) }
  ","                               { \p s -> Comma (getPosition p) }
  "."                               { \p s -> Dot (getPosition p) }
  ";"                               { \p s -> EndCommand (getPosition p) }
  $digit+"."$digit+                 { \p s -> REAL (getPosition p) (read s) }
  $digit+                           { \p s -> INTEIRO (getPosition p) (read s) }
  \'.\'                             { \p s -> CARACTERE (getPosition p) (read s) }
  \".*\"                            { \p s -> TEXTO (getPosition p) (read s) }
  $upperalpha [$upperalpha \_]*     { \p s -> TIPO (getPosition p) s }
  $loweralpha [$alpha \_]*          { \p s -> ID (getPosition p) s }

{

-- The Token type:
data Token =
    ESTRUTURA (Int,Int)             |
    FIMESTRUTURA (Int,Int)          |
    FUNCAO (Int,Int)                |
    FIMFUNCAO (Int,Int)             |
    PROCEDIMENTO (Int,Int)          |
    FIMPROCEDIMENTO (Int,Int)       |
    OPERADOR (Int,Int)              |
    FIMOPERADOR (Int,Int)           |
    RECEBE (Int,Int)                |
    RETORNA (Int,Int)               |
    RETORNE (Int,Int)               |
    PRINCIPAL (Int,Int)             |
    FIMPRINCIPAL (Int,Int)          |
    SAIA (Int,Int)                  |
    CONTINUE (Int,Int)              |
    SE (Int,Int)                    |
    ENTAO (Int,Int)                 |
    SENAO (Int,Int)                 |
    FIMSE (Int,Int)                 |
    ENQUANTO (Int,Int)              |
    EXECUTE (Int,Int)               |
    FIMENQUANTO (Int,Int)           |
    DEFINA (Int,Int)                |
    SlowOU (Int,Int)                |
    SlowE (Int,Int)                 |
    OU (Int,Int)                    |
    E (Int,Int)                     |
    LOGICO (Int,Int) Bool           |
    PONT (Int,Int)                  |
    NOVO (Int,Int)                  |
    DELETE (Int,Int)                |
    CONST (Int,Int)                 |
    Attrib (Int,Int)                |
    Geq (Int,Int)                   |
    Leq (Int,Int)                   |
    Diff (Int,Int)                  |
    Equal (Int,Int)                 |
    Great (Int,Int)                 |
    Less (Int,Int)                  |
    Add (Int,Int)                   |
    Sub (Int,Int)                   |
    Mult (Int,Int)                  |
    Div (Int,Int)                   |
    MOD (Int,Int)                   |
    NOT (Int,Int)                   |
    OpenBrack (Int,Int)             |
    CloseBrack (Int,Int)            |
    OpenSqBrack (Int,Int)           |
    CloseSqBrack (Int,Int)          |
    Comma (Int,Int)                 |
    Dot (Int,Int)                   |
    EndCommand (Int,Int)            |
    REAL (Int,Int) Float            |
    INTEIRO (Int,Int) Integer       |
    CARACTERE (Int,Int) Char        |
    TEXTO (Int,Int) String          |
    TIPO (Int,Int) String           |
    ID (Int,Int) String
    deriving (Eq,Show)

-- Receives a AlexPosn and returns a pair (Int,Int) with (row,column) positions.
getPosition :: AlexPosn -> (Int,Int)
getPosition (AlexPn _ a b) = (a,b)

-- It's only called when Alex is getting the LOGICO tokens.
getBoolValue :: String -> Bool
getBoolValue str
    | str == "VERDADEIRO" = True
    | otherwise = False

-- Receives a Token and returns its position.
token_pos :: Token -> (Int,Int)
token_pos (ESTRUTURA p) = p
token_pos (FIMESTRUTURA p) = p
token_pos (FUNCAO p) = p
token_pos (FIMFUNCAO p) = p
token_pos (PROCEDIMENTO p) = p
token_pos (FIMPROCEDIMENTO p) = p
token_pos (OPERADOR p) = p
token_pos (FIMOPERADOR p) = p
token_pos (RECEBE p) = p
token_pos (RETORNA p) = p
token_pos (RETORNE p) = p
token_pos (PRINCIPAL p) = p
token_pos (FIMPRINCIPAL p) = p
token_pos (SAIA p) = p
token_pos (CONTINUE p) = p
token_pos (SE p) = p
token_pos (ENTAO p) = p
token_pos (SENAO p) = p
token_pos (FIMSE p) = p
token_pos (ENQUANTO p) = p
token_pos (EXECUTE p) = p
token_pos (FIMENQUANTO p) = p
token_pos (DEFINA p) = p
token_pos (SlowOU p) = p
token_pos (SlowE p) = p
token_pos (OU p) = p
token_pos (E p) = p
token_pos (LOGICO p _) = p
token_pos (PONT p) = p
token_pos (NOVO p) = p
token_pos (DELETE p) = p
token_pos (CONST p) = p
token_pos (Attrib p) = p
token_pos (Geq p) = p
token_pos (Leq p) = p
token_pos (Diff p) = p
token_pos (Equal p) = p
token_pos (Great p) = p
token_pos (Less p) = p
token_pos (Add p) = p
token_pos (Sub p) = p
token_pos (Mult p) = p
token_pos (Div p) = p
token_pos (MOD p) = p
token_pos (NOT p) = p
token_pos (OpenBrack p) = p
token_pos (CloseBrack p) = p
token_pos (OpenSqBrack p) = p
token_pos (CloseSqBrack p) = p
token_pos (Comma p) = p
token_pos (Dot p) = p
token_pos (EndCommand p) = p
token_pos (REAL p _) = p
token_pos (INTEIRO p _) = p
token_pos (CARACTERE p _) = p
token_pos (TEXTO p _) = p
token_pos (TIPO p _) = p
token_pos (ID p _) = p

-- Receives a file name and returns all Tokens present in this file
getTokens :: String -> [Token]
getTokens fn = unsafePerformIO (getTokensAux fn)

-- Assists getTokens to compute Tokens from the file.
getTokensAux :: String -> IO [Token]
getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}
