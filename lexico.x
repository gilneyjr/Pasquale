{
  module Main (main, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$loweralpha = [a-z]  -- lowercase alphabetic characters
$upperalpha = [A-Z]  -- uppercase alphabetic characters


tokens :-
  $white+								;
  "--".*.								;
  ESTRUTURA								{ \p s -> ESTRUTURA p }
  FIMESTRUTURA							{ \p s -> FIMESTRUTURA p }
  FUNCAO								{ \p s -> FUNCAO p }
  FIMFUNCAO								{ \p s -> FIMFUNCAO p }
  PROCEDIMENTO							{ \p s -> PROCEDIMENTO p }
  FIMPROCEDIMENTO						{ \p s -> FIMPROCEDIMENTO p }
  OPERADOR								{ \p s -> OPERADOR p }
  FIMOPERADOR							{ \p s -> FIMOPERADOR p }
  RECEBE								{ \p s -> RECEBE p }
  RETORNA								{ \p s -> RETORNA p }
  RETORNE								{ \p s -> RETORNE p }
  PRINCIPAL								{ \p s -> PRINCIPAL p }
  FIMPRINCIPAL							{ \p s -> FIMPRINCIPAL p }
  SAIA									{ \p s -> SAIA p }
  CONTINUE								{ \p s -> CONTINUE p }
  SE									{ \p s -> SE p }
  ENTAO									{ \p s -> ENTAO p }
  SENAO									{ \p s -> SENAO p }
  FIMSE									{ \p s -> FIMSE p }
  ENQUANTO								{ \p s -> ENQUANTO p }
  EXECUTE								{ \p s -> EXECUTE p }
  FIMENQUANTO							{ \p s -> FIMENQUANTO p }
  DEFINA								{ \p s -> DEFINA p }
  "~OU"									{ \p s -> SlowOU p }
  "~E"									{ \p s -> SlowE p }
  OU									{ \p s -> OU p }
  E										{ \p s -> E p }
  VERDADEIRO							{ \p s -> BoolValue p s }
  FALSO									{ \p s -> BoolValue p s }
  PONT									{ \p s -> PONT p }
  NOVO									{ \p s -> NOVO p }
  DELETE								{ \p s -> DELETE p }
  CONST									{ \p s -> CONST p }
  ":="									{ \p s -> Attrib p }
  ">="									{ \p s -> Geq p }
  "<="									{ \p s -> Leq p }
  "/="									{ \p s -> Diff p }
  "="									{ \p s -> Equal p }
  ">"									{ \p s -> Great p }
  "<"									{ \p s -> Less p }
  "+"									{ \p s -> Add p }
  "-"									{ \p s -> Sub p }
  "*"									{ \p s -> Mult p }
  "/"									{ \p s -> Div p }
  "MOD"									{ \p s -> MOD p }
  "!"									{ \p s -> NOT p }
  "("									{ \p s -> OpenBrack p }
  ")"									{ \p s -> CloseBrack p }
  "["									{ \p s -> OpenSqBrack p }
  "]"									{ \p s -> CloseSqBrack p }
  ";"									{ \p s -> EndCommand p }
  $digit+								{ \p s -> INTEIRO p (read s) }
  \".*\"								{ \p s -> TEXTO p s }
  $upperalpha [$upperalpha \_]*			{ \p s -> TIPO p s }
  $loweralpha [$alpha \_]*				{ \p s -> ID p s }
  .*									;

{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
    ESTRUTURA AlexPosn                  |
    FIMESTRUTURA AlexPosn               |
    FUNCAO AlexPosn                     |
    FIMFUNCAO AlexPosn                  |
    PROCEDIMENTO AlexPosn               |
    FIMPROCEDIMENTO AlexPosn            |
    OPERADOR AlexPosn                   |
    FIMOPERADOR AlexPosn                |
    RECEBE AlexPosn                     |
    RETORNA AlexPosn                    |
    RETORNE AlexPosn                    |
    PRINCIPAL AlexPosn                  |
    FIMPRINCIPAL AlexPosn               |
    SAIA AlexPosn                       |
    CONTINUE AlexPosn                   |
    SE AlexPosn                         |
    ENTAO AlexPosn                      |
    SENAO AlexPosn                      |
    FIMSE AlexPosn                      |
    ENQUANTO AlexPosn                   |
    EXECUTE AlexPosn                    |
    FIMENQUANTO AlexPosn                |
    SlowOU AlexPosn                     |
    SlowE AlexPosn                      |
    OU AlexPosn                         |
    E AlexPosn                          |
    BoolValue AlexPosn String           |
    PONT AlexPosn                       |
    NOVO AlexPosn                       |
    DELETE AlexPosn						|
    CONST AlexPosn                  	|
    Attrib AlexPosn                     |
    Geq AlexPosn						|
    Leq AlexPosn						|
    Diff AlexPosn						|
    Equal AlexPosn						|
    Great AlexPosn						|
    Less AlexPosn						|
    Add AlexPosn						|
    Sub AlexPosn						|
    Mult AlexPosn						|
    Div AlexPosn						|
    MOD AlexPosn						|
    NOT AlexPosn                        |
    OpenBrack AlexPosn                  |
    CloseBrack AlexPosn                 |
    OpenSqBrack AlexPosn                |
    CloseSqBrack AlexPosn               |
    EndCommand AlexPosn                 |
    INTEIRO AlexPosn Integer            |
    TEXTO AlexPosn String               |
    TIPO AlexPosn String                |
    ID AlexPosn String                        
    deriving (Eq,Show)

token_posn (ESTRUTURA p) = p
token_posn (FIMESTRUTURA p) = p
token_posn (FUNCAO p) = p
token_posn (FIMFUNCAO p) = p
token_posn (PROCEDIMENTO p) = p
token_posn (FIMPROCEDIMENTO p) = p
token_posn (OPERADOR p) = p
token_posn (FIMOPERADOR p) = p
token_posn (RECEBE p) = p
token_posn (RETORNA p) = p
token_posn (RETORNE p) = p
token_posn (PRINCIPAL p) = p
token_posn (FIMPRINCIPAL p) = p
token_posn (SAIA p) = p
token_posn (CONTINUE p) = p
token_posn (SE p) = p
token_posn (ENTAO p) = p
token_posn (SENAO p) = p
token_posn (FIMSE p) = p
token_posn (ENQUANTO p) = p
token_posn (EXECUTE p) = p
token_posn (FIMENQUANTO p) = p
token_posn (SlowOU p) = p
token_posn (SlowE p) = p
token_posn (OU p) = p
token_posn (E p) = p
token_posn (BoolValue p _) = p
token_posn (PONT p) = p
token_posn (NOVO p) = p
token_posn (DELETE p) = p
token_posn (CONST p) = p
token_posn (Attrib p) = p
token_posn (Geq p) = p
token_posn (Leq p) = p
token_posn (Diff p) = p
token_posn (Equal p) = p
token_posn (Great p) = p
token_posn (Less p) = p
token_posn (Add p) = p
token_posn (Sub p) = p
token_posn (Mult p) = p
token_posn (Div p) = p
token_posn (MOD p) = p
token_posn (NOT p) = p
token_posn (OpenBrack p) = p
token_posn (CloseBrack p) = p
token_posn (OpenSqBrack p) = p
token_posn (CloseSqBrack p) = p
token_posn (EndCommand p) = p
token_posn (INTEIRO p _) = p
token_posn (TEXTO p _) = p
token_posn (TIPO p _) = p
token_posn (ID p _) = p

main = do
  s <- getContents
  print (alexScanTokens s)
}
