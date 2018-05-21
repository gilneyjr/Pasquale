{
  module Main (main, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters

tokens :-

  $white+							;
  "/*" .* "*/"						;
  "//".* 							;
  "(" 								{ \p s -> LPar p }
  ")" 								{ \p s -> RPar p }
  "[" 								{ \p s -> LSBra p }
  "]" 								{ \p s -> RSBra p }
  "{" 								{ \p s -> LCBra p }
  "}" 								{ \p s -> RCBra p }
  ";" 								{ \p s -> SemiColon p }
  "=" 								{ \p s -> Assign p }
  "," 								{ \p s -> Comma p }
  continue 							{ \p s -> Continue p }
  return 							{ \p s -> Return p }
  break 							{ \p s -> Break p }
  for 								{ \p s -> For p }
  while 							{ \p s -> While p }
  if 								{ \p s -> If p }
  else 								{ \p s -> Else p }
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
  LSBra AlexPosn |
  RSBra AlexPosn |
  LCBra AlexPosn |
  RCBra AlexPosn 
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
