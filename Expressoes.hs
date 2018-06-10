module Expressoes where

import Data.Fixed
import Data.List
import Data.Maybe
import System.IO
import Tipos
import Estado
import Lexico
import Arvore

getposTokenOp :: Token -> (Int,Int)
getposTokenOp (Add p) = p
getposTokenOp (Sub p) = p
getposTokenOp (Mult p) = p
getposTokenOp (Div p) = p
getposTokenOp (Geq p) = p
getposTokenOp (Leq p) = p
getposTokenOp (Diff p) = p
getposTokenOp (Equal p) = p
getposTokenOp (Great p) = p
getposTokenOp (Less p) = p

evaluateExpr :: Estado -> EXPR -> (Valor,Estado)

evaluateExpr estado (CRIAOU expr1 op expr2) = do
    case res1 of
        ValorLogico True -> (ValorLogico True, estado1)
        ValorLogico False ->
            case res2 of
                ValorLogico a -> (ValorLogico a, estado2)
                otherwise -> error $ "Tipos inválidos para o comando OU : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando OU : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIASLOWOU expr1 op expr2) = do
    case res1 of
        ValorLogico a ->
            case res2 of
                ValorLogico b -> (ValorLogico (a || b), estado2)
                otherwise -> error $ "Tipos inválidos para o comando OU~ : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando OU~ : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAE expr1 op expr2) = do
    case res1 of
        ValorLogico False -> (ValorLogico False, estado1)
        ValorLogico True ->
            case res2 of
                ValorLogico a -> (ValorLogico a, estado2)
                otherwise -> error $ "Tipos inválidos para o comando E : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando E : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIASLOWE expr1 op expr2) = do
    case res1 of
        ValorLogico a ->
            case res2 of
                ValorLogico b -> (ValorLogico (a && b), estado2)
                otherwise -> error $ "Tipos inválidos para o comando E~ : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando E~ : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2


evaluateExpr estado (CRIALESS expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a < b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador < : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a < b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador < : posição " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a < b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador < : posição " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a < b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador < : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador < : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIALEQ expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a <= b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador <= : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a <= b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador <= : posição " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a <= b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador <= : posição " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a <= b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador <= : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador <= : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAEQUAL expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a == b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador = : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a == b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador = : posição " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a == b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador = : posição " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a == b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador = : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador = : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAGEQ expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a >= b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador >= : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a >= b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador >= : posição " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a >= b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador >= : posição " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a >= b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador >= : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador >= : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAGREAT expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a > b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador > : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a > b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador > : posição " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a > b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador > : posição " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a > b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador > : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador > : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIADIFF expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a /= b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador /= : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a /= b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador /= : posição " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a /= b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador /= : posição " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a /= b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador /= : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador /= : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAADD expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (a + b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador + : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a + b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador + : posição " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorTexto (a ++ b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador + : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador + : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIASUB expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (a - b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador - : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a - b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador - : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador - : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2 

evaluateExpr estado (CRIAMULT expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (a * b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador * : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a * b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador * : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador * : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2 

evaluateExpr estado (CRIADIV expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (quot a b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador / : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a / b), estado2)
                otherwise -> error $ "Tipos inválidos para o operador / : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador / : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2 
        
evaluateExpr estado (CRIAMOD expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (mod a b), estado2)
                otherwise -> error $ "Tipos inválidos para o comando MOD : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando MOD : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr1
        (res2,estado2) = evaluateExpr estado1 expr2 

evaluateExpr estado (CRIANEG op expr) = do
    case res1 of
        ValorInteiro a -> (ValorInteiro (-a), estado1)
        ValorReal a -> (ValorReal (-a), estado1)
        otherwise -> error $ "Tipo inválido para o operador - : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr

evaluateExpr estado (CRIANOT op expr) = do
    case res1 of
        ValorLogico a -> (ValorLogico (not a), estado1)
        otherwise -> error $ "Tipo inválido para o operador ! : posição " ++ show (getposTokenOp op)
    where
        (res1,estado1) = evaluateExpr estado expr

evaluateExpr estado (CRIACONVERSAO tipo expr) = do
    case tipo of
        TIPO _ "INTEIRO" -> 
            case res1 of
                ValorInteiro a -> (ValorInteiro a, estado1)
                ValorReal a -> (ValorInteiro (read (show a)), estado1)
                ValorCaractere a -> (ValorInteiro (read (show a)), estado1)
                ValorTexto a -> (ValorInteiro (read a), estado1)
                ValorLogico a -> (ValorInteiro (logicoToInt a), estado1)
                otherwise -> error $ "Conversao inválida : posição " ++ show (getposTokenTipo tipo)
        TIPO _ "REAL" -> 
            case res1 of
                ValorInteiro a -> (ValorReal (read (show a)), estado1)
                ValorReal a -> (ValorReal a, estado1)
                ValorCaractere a -> (ValorReal (read (show a)), estado1)
                ValorTexto a -> (ValorReal (read a), estado1)
                ValorLogico a -> (ValorReal (logicoToReal a), estado1)
                otherwise -> error $ "Conversao inválida : posição " ++ show (getposTokenTipo tipo)
        TIPO _ "TEXTO" ->
            case res1 of
                ValorInteiro a -> (ValorTexto (show a), estado1)
                ValorReal a -> (ValorTexto (show a), estado1)
                ValorCaractere a -> (ValorTexto (show a), estado1)
                ValorTexto a -> (ValorTexto a, estado1)
                ValorLogico a -> (ValorTexto (showLogico a), estado1)
                otherwise -> error $ "Conversao inválida : posição " ++ show (getposTokenTipo tipo)
        TIPO _ "CARACTERE" ->
            case res1 of
                ValorCaractere a -> (ValorCaractere a, estado1)
                otherwise -> error $ "Conversao inválida : posição " ++ show (getposTokenTipo tipo)
        TIPO _ "LOGICO" ->
            case res1 of
                ValorInteiro a -> (ValorLogico (intToLogico a), estado1)
                ValorReal a -> (ValorLogico (realToLogico a), estado1)
                ValorLogico a -> (ValorLogico a, estado1)
                otherwise -> error $ "Conversao inválida : posição " ++ show (getposTokenTipo tipo)
        otherwise -> error $ "Conversao inválida : posição " ++ show (getposTokenTipo tipo)
    where
        (res1,estado1) = evaluateExpr estado expr
        getposTokenTipo :: Token -> (Int,Int)
        getposTokenTipo (TIPO p _) = p
        showLogico :: Bool -> String
        showLogico True = "VERDADEIRO"
        showLogico False = "FALSO"
        logicoToInt :: Bool -> Integer
        logicoToInt False = 0
        logicoToInt True = 1
        logicoToReal :: Bool -> Double
        logicoToReal False = 0
        logicoToReal True = 1
        intToLogico :: Integer -> Bool
        intToLogico 0 = False
        intToLogico _ = True
        realToLogico :: Double -> Bool
        realToLogico 0 = False
        realToLogico _ = True
        
evaluateExpr estado (CRIATEXTO (TEXTO _ t)) = (ValorTexto t, estado) -- OK
evaluateExpr estado (CRIAINT (INTEIRO _ i)) = (ValorInteiro i, estado) -- OK
evaluateExpr estado (CRIACARACTERE (CARACTERE _ c)) = (ValorCaractere c, estado) -- OK
evaluateExpr estado (CRIALOGICO (LOGICO _ l)) = (ValorLogico l, estado) -- OK
evaluateExpr estado (CRIAREAL (REAL _ r)) = (ValorReal r, estado) -- OK
evaluateExpr estado (CRIAPARENTESES a) = evaluateExpr estado a -- OK

{-
    CRIAVAR VAR |
    CRIACHAMADAFUNC CHAMADA |
    CRIANOVO [PONT] {-TIPO-}Token OptionalSQBRACK |
    CRIAVALOREXPR VAL |
    CRIACONVERSAO {-TIPO-}Token EXPR
-}

