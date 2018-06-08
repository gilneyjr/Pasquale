{
module Lexico where
import System.IO.Unsafe
import System.IO
}

-- Using the "posn" wrapper
%wrapper "posn"

$digit = 0-9         -- digits
$alpha = [a-zA-Z]    -- alphabetic characters
$loweralpha = a-z  -- lowercase alphabetic characters
$upperalpha = A-Z  -- uppercase alphabetic characters
@string = \" [^\"]* \"

-- Regular expressions that define the language tokens.
tokens :-
  $white+                           ;
  "//".*                            ;
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
  BLOCO                             { \p s -> BLOCO (getPosition p) }
  FIMBLOCO                          { \p s -> FIMBLOCO (getPosition p) }
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
  PONTEIRO                          { \p s -> PONTEIRO (getPosition p) }
  NOVO                              { \p s -> NOVO (getPosition p) }
  DELETE                            { \p s -> DELETE (getPosition p) }
  ESCREVA                           { \p s -> ESCREVA (getPosition p) }
  LEIA                              { \p s -> LEIA (getPosition p) }
  VALOR                             { \p s -> VALOR (getPosition p) }
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
  @string                           { \p s -> TEXTO (getPosition p) (read s) }
  $upperalpha [$alpha \_ $digit]*   { \p s -> TIPO (getPosition p) s }
  $loweralpha [$alpha \_ $digit]*   { \p s -> ID (getPosition p) s }
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
    BLOCO (Int,Int)                 |
    FIMBLOCO (Int,Int)              |
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
    PONTEIRO (Int,Int)              |
    NOVO (Int,Int)                  |
    DELETE (Int,Int)                |
    LEIA (Int,Int)                  |
    ESCREVA (Int,Int)               |
    VALOR (Int,Int)                 |
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
    REAL (Int,Int) Double           |
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

getTokens :: String -> [Token]
getTokens fn = unsafePerformIO (getTokensAux fn)

-- Assists getTokens to compute Tokens from the String.
getTokensAux :: String -> IO [Token]
getTokensAux fn = return $ alexScanTokens fn

}
