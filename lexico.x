{
  module Main (main, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters

tokens :-

  $white+							          ;
  "(" 								          { \p s -> LPar p }
  ")" 								          { \p s -> RPar p }
  "[" 								          { \p s -> LColch p }
  "]" 								          { \p s -> RColch p }
  "{" 								          { \p s -> LChave p }
  "}" 								          { \p s -> RChave p }
  ";" 								          { \p s -> PontVirg p }
  ":=" 								          { \p s -> Recebe p }
  "," 							       	    { \p s -> Virg p }
  CONTINUE 							        { \p s -> Continue p }
  RETORNE 							        { \p s -> Retorne p }
  SAIA 							            { \p s -> Saia p }
  ENQUANTO         							{ \p s -> Enquanto p }
  SE 								            { \p s -> Se p }
  ENTAO                         { \p s -> Entao p }
  else 								          { \p s -> Else p }
  struct 							{ \p s -> Struct p }
  int 									{ \p s -> Int p }
  float 								{ \p s -> Float p }
  string 								{ \p s -> String p }
  char 									{ \p s -> Char p }
  ["&" "|"] 							{ \p s -> Logical p s }
  ["==" ">=" "<=" "<" ">"] 				{ \p s -> Relational p s }
  ["+" "-"]								{ \p s -> SumOp p s }
  ["*" "/" "%"] 						{ \p s -> MulOp p s }
  $alpha [$alpha $digit \_ ]*       	{ \p s -> Name p s }
  $ \" alpha [$alpha $digit \_ ]* \"      { \p s -> Name p s }
{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  LPar AlexPosn |
  RPar AlexPosn |
  LColch AlexPosn |
  RColch AlexPosn |
  LChave AlexPosn |
  RChave AlexPosn 
  deriving (Eq,Show)

token_posn (Let p) = p
token_posn (In p) = p
token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p

main = do
  s <- getContents
  print (alexScanTokens s)
}
