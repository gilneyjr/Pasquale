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
@string = \"[^\"\\]*(\\.[^\"\\]*)*\"
@char = \' ("\n"|"\t"|"\r"|"\0"|"\'"|\\\\|.) \'
@comments = "/*" (.*|\n)* "*/"

-- Regular expressions that define the language tokens.
tokens :-
  $white+                           ;
  "//".*                            ;
  @comments                         ;
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
  NULO                              { \p s -> NULO (getPosition p) }
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
  "!"                               { \p s -> Not (getPosition p) }
  "("                               { \p s -> OpenBrack (getPosition p) }
  ")"                               { \p s -> CloseBrack (getPosition p) }
  "["                               { \p s -> OpenSqBrack (getPosition p) }
  "]"                               { \p s -> CloseSqBrack (getPosition p) }
  ","                               { \p s -> Comma (getPosition p) }
  "."                               { \p s -> Dot (getPosition p) }
  ";"                               { \p s -> EndCommand (getPosition p) }
  $digit+"."$digit+                 { \p s -> REAL (getPosition p) (read s) }
  $digit+                           { \p s -> INTEIRO (getPosition p) (read s) }
  @char                             { \p s -> CARACTERE (getPosition p) (read s) }
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
    NULO (Int,Int)                  |
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
    Not (Int,Int)                   |
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
    deriving (Eq)

instance Show Token where
    show (ESTRUTURA p)            = ( "\"ESTRUTURA\" " ++ (show p))
    show (FIMESTRUTURA p)         = ( "\"FIMESTRUTURA\" " ++ (show p))
    show (FUNCAO p)               = ( "\"FUNCAO\" " ++ (show p))
    show (FIMFUNCAO p)            = ( "\"FIMFUNCAO\" " ++ (show p))
    show (PROCEDIMENTO p)         = ( "\"PROCEDIMENTO\" " ++ (show p))
    show (FIMPROCEDIMENTO p)      = ( "\"FIMPROCEDIMENTO\" " ++ (show p))
    show (OPERADOR p)             = ( "\"OPERADOR\" " ++ (show p))
    show (FIMOPERADOR p)          = ( "\"FIMOPERADOR\" " ++ (show p))
    show (RECEBE p)               = ( "\"RECEBE\" " ++ (show p))
    show (RETORNA p)              = ( "\"RETORNA\" " ++ (show p))
    show (RETORNE p)              = ( "\"RETORNE\" " ++ (show p))
    show (PRINCIPAL p)            = ( "\"PRINCIPAL\" " ++ (show p))
    show (FIMPRINCIPAL p)         = ( "\"FIMPRINCIPAL\" " ++ (show p))
    show (BLOCO p)                = ( "\"BLOCO\" " ++ (show p))
    show (FIMBLOCO p)             = ( "\"FIMBLOCO\" " ++ (show p))
    show (SAIA p)                 = ( "\"SAIA\" " ++ (show p))
    show (CONTINUE p)             = ( "\"CONTINUE\" " ++ (show p))
    show (SE p)                   = ( "\"SE\" " ++ (show p))
    show (ENTAO p)                = ( "\"ENTAO\" " ++ (show p))
    show (SENAO p)                = ( "\"SENAO\" " ++ (show p))
    show (FIMSE p)                = ( "\"FIMSE\" " ++ (show p))
    show (ENQUANTO p)             = ( "\"ENQUANTO\" " ++ (show p))
    show (EXECUTE p)              = ( "\"EXECUTE\" " ++ (show p))
    show (FIMENQUANTO p)          = ( "\"FIMENQUANTO\" " ++ (show p))
    show (SlowOU p)               = ( "\"~OU\" " ++ (show p))
    show (SlowE p)                = ( "\"~E\" " ++ (show p))
    show (OU p)                   = ( "\"OU\" " ++ (show p))
    show (E p)                    = ( "\"E\" " ++ (show p))
    show (LOGICO p v)             = ( "\"" ++ (getLogicoName v) ++ "\" " ++ (show p))
    show (PONTEIRO p)             = ( "\"PONTEIRO\" " ++ (show p))
    show (NOVO p)                 = ( "\"NOVO\" " ++ (show p))
    show (DELETE p)               = ( "\"DELETE\" " ++ (show p))
    show (LEIA p)                 = ( "\"LEIA\" " ++ (show p))
    show (ESCREVA p)              = ( "\"ESCREVA\" " ++ (show p))
    show (VALOR p)                = ( "\"VALOR\" " ++ (show p))
    show (NULO p)                 = ( "\"NULO\" " ++ (show p))
    show (Attrib p)               = ( "\":=\" " ++ (show p))
    show (Geq p)                  = ( "\">=\" " ++ (show p))
    show (Leq p)                  = ( "\"<=\" " ++ (show p))
    show (Diff p)                 = ( "\"/=\" " ++ (show p))
    show (Equal p)                = ( "\"=\" " ++ (show p))
    show (Great p)                = ( "\">\" " ++ (show p))
    show (Less p)                 = ( "\"<\" " ++ (show p))
    show (Add p)                  = ( "\"+\" " ++ (show p))
    show (Sub p)                  = ( "\"-\" " ++ (show p))
    show (Mult p)                 = ( "\"*\" " ++ (show p))
    show (Div p)                  = ( "\"/\" " ++ (show p))
    show (MOD p)                  = ( "\"MOD\" " ++ (show p))
    show (Not p)                  = ( "\"!\" " ++ (show p))
    show (OpenBrack p)            = ( "\"(\" " ++ (show p))
    show (CloseBrack p)           = ( "\")\" " ++ (show p))
    show (OpenSqBrack p)          = ( "\"[\" " ++ (show p))
    show (CloseSqBrack p)         = ( "\"]\" " ++ (show p))
    show (Comma p)                = ( "\",\" " ++ (show p))
    show (Dot p)                  = ( "\".\" " ++ (show p))
    show (EndCommand p)           = ( "\";\" " ++ (show p))
    show (REAL p v)               = ( "\"" ++ (show v) ++ "\" " ++ (show p))
    show (INTEIRO p v)            = ( "\"" ++ (show v) ++ "\" " ++ (show p))
    show (CARACTERE p v)          = ( "\"" ++ (show v) ++ "\" " ++ (show p))
    show (TEXTO p v)              = ( "\"" ++ v ++ "\" " ++ (show p))
    show (TIPO p v)               = ( "Tipo \"" ++ v ++ "\" " ++ (show p))
    show (ID p v)                 = ( "\""++ v ++ "\" " ++ (show p))
    
    
-- Receives a AlexPosn and returns a pair (Int,Int) with (row,column) positions.
getPosition :: AlexPosn -> (Int,Int)
getPosition (AlexPn _ a b) = (a,b)

-- It's only called when Alex is getting the LOGICO tokens.
getBoolValue :: String -> Bool
getBoolValue str
    | str == "VERDADEIRO" = True
    | otherwise = False

getLogicoName:: Bool -> String
getLogicoName val
    | val == True = "VERDADEIRO"
    | otherwise = "FALSO"

getTokens :: String -> [Token]
getTokens = alexScanTokens

}
