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

evaluateExpr :: Estado -> EXPR -> (Valor,Tipo,Estado)

evaluateExpr estado (CRIAOU expr1 op expr2) = do
    case res1 of
        ValorLogico True -> (ValorLogico True, TipoAtomico "LOGICO", estado1)
        ValorLogico False ->
            case res2 of
                ValorLogico a -> (ValorLogico a, TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando OU : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando OU : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIASLOWOU expr1 op expr2) = do
    case res1 of
        ValorLogico a ->
            case res2 of
                ValorLogico b -> (ValorLogico (a || b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando OU~ : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando OU~ : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAE expr1 op expr2) = do
    case res1 of
        ValorLogico False -> (ValorLogico False, TipoAtomico "LOGICO", estado1)
        ValorLogico True ->
            case res2 of
                ValorLogico a -> (ValorLogico a, TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando E : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando E : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIASLOWE expr1 op expr2) = do
    case res1 of
        ValorLogico a ->
            case res2 of
                ValorLogico b -> (ValorLogico (a && b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando E~ : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando E~ : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2


evaluateExpr estado (CRIALESS expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a < b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador < : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a < b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador < : posição " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a < b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador < : posição " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a < b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador < : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador < : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIALEQ expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a <= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador <= : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a <= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador <= : posição " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a <= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador <= : posição " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a <= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador <= : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador <= : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAEQUAL expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a == b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador = : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a == b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador = : posição " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a == b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador = : posição " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a == b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador = : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador = : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAGEQ expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a >= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador >= : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a >= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador >= : posição " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a >= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador >= : posição " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a >= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador >= : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador >= : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAGREAT expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a > b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador > : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a > b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador > : posição " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a > b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador > : posição " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a > b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador > : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador > : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIADIFF expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a /= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador /= : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a /= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador /= : posição " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a /= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador /= : posição " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a /= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador /= : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador /= : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAADD expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (a + b), TipoAtomico "INTEIRO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador + : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a + b), TipoAtomico "REAL", estado2)
                otherwise -> error $ "Tipos inválidos para o operador + : posição " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorTexto (a ++ b), TipoAtomico "TEXTO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador + : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador + : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIASUB expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (a - b), TipoAtomico "INTEIRO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador - : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a - b), TipoAtomico "REAL", estado2)
                otherwise -> error $ "Tipos inválidos para o operador - : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador - : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2 

evaluateExpr estado (CRIAMULT expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (a * b), TipoAtomico "INTEIRO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador * : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a * b), TipoAtomico "REAL", estado2)
                otherwise -> error $ "Tipos inválidos para o operador * : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador * : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2 

evaluateExpr estado (CRIADIV expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (quot a b), TipoAtomico "INTEIRO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador / : posição " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a / b), TipoAtomico "REAL", estado2)
                otherwise -> error $ "Tipos inválidos para o operador / : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador / : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2 
        
evaluateExpr estado (CRIAMOD expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (mod a b), TipoAtomico "INTEIRO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando MOD : posição " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando MOD : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr1
        (res2,_,estado2) = evaluateExpr estado1 expr2 

evaluateExpr estado (CRIANEG op expr) = do
    case res1 of
        ValorInteiro a -> (ValorInteiro (-a), TipoAtomico "INTEIRO", estado1)
        ValorReal a -> (ValorReal (-a), TipoAtomico "REAL", estado1)
        otherwise -> error $ "Tipo inválido para o operador - : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr

evaluateExpr estado (CRIANOT op expr) = do
    case res1 of
        ValorLogico a -> (ValorLogico (not a), TipoAtomico "LOGICO", estado1)
        otherwise -> error $ "Tipo inválido para o operador ! : posição " ++ show (getposTokenOp op)
    where
        (res1,_,estado1) = evaluateExpr estado expr

evaluateExpr estado (CRIACONVERSAO tipo expr) = do
    case tipo of
        TIPO _ "INTEIRO" -> 
            case res1 of
                ValorInteiro a -> (ValorInteiro a, TipoAtomico "INTEIRO", estado1)
                ValorReal a -> (ValorInteiro (read (show a)), TipoAtomico "INTEIRO", estado1)
                ValorCaractere a -> (ValorInteiro (read (show a)), TipoAtomico "INTEIRO", estado1)
                ValorTexto a -> (ValorInteiro (read a), TipoAtomico "INTEIRO", estado1)
                ValorLogico a -> (ValorInteiro (logicoToInt a), TipoAtomico "INTEIRO", estado1)
                otherwise -> error $ "Conversao inválida : posição " ++ show (getposTokenTipo tipo)
        TIPO _ "REAL" -> 
            case res1 of
                ValorInteiro a -> (ValorReal (read (show a)), TipoAtomico "REAL", estado1)
                ValorReal a -> (ValorReal a, TipoAtomico "REAL", estado1)
                ValorCaractere a -> (ValorReal (read (show a)), TipoAtomico "REAL", estado1)
                ValorTexto a -> (ValorReal (read a), TipoAtomico "REAL", estado1)
                ValorLogico a -> (ValorReal (logicoToReal a), TipoAtomico "REAL", estado1)
                otherwise -> error $ "Conversao inválida : posição " ++ show (getposTokenTipo tipo)
        TIPO _ "TEXTO" ->
            case res1 of
                ValorInteiro a -> (ValorTexto (show a), TipoAtomico "TEXTO", estado1)
                ValorReal a -> (ValorTexto (show a), TipoAtomico "TEXTO", estado1)
                ValorCaractere a -> (ValorTexto (show a), TipoAtomico "TEXTO", estado1)
                ValorTexto a -> (ValorTexto a, TipoAtomico "TEXTO", estado1)
                ValorLogico a -> (ValorTexto (showLogico a), TipoAtomico "TEXTO", estado1)
                otherwise -> error $ "Conversao inválida : posição " ++ show (getposTokenTipo tipo)
        TIPO _ "CARACTERE" ->
            case res1 of
                ValorCaractere a -> (ValorCaractere a, TipoAtomico "CARACTERE", estado1)
                otherwise -> error $ "Conversao inválida : posição " ++ show (getposTokenTipo tipo)
        TIPO _ "LOGICO" ->
            case res1 of
                ValorInteiro a -> (ValorLogico (intToLogico a), TipoAtomico "LOGICO", estado1)
                ValorReal a -> (ValorLogico (realToLogico a), TipoAtomico "LOGICO", estado1)
                ValorLogico a -> (ValorLogico a, TipoAtomico "LOGICO", estado1)
                otherwise -> error $ "Conversao inválida : posição " ++ show (getposTokenTipo tipo)
        otherwise -> error $ "Conversao inválida : posição " ++ show (getposTokenTipo tipo)
    where
        (res1,_,estado1) = evaluateExpr estado expr
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
        
evaluateExpr estado (CRIATEXTO (TEXTO _ t)) = (ValorTexto t, TipoAtomico "TEXTO", estado)
evaluateExpr estado (CRIAINT (INTEIRO _ i)) = (ValorInteiro i, TipoAtomico "INTEIRO", estado)
evaluateExpr estado (CRIACARACTERE (CARACTERE _ c)) = (ValorCaractere c, TipoAtomico "CARACTERE", estado)
evaluateExpr estado (CRIALOGICO (LOGICO _ l)) = (ValorLogico l, TipoAtomico "LOGICO", estado)
evaluateExpr estado (CRIAREAL (REAL _ r)) = (ValorReal r, TipoAtomico "REAL", estado)
evaluateExpr estado (CRIAPARENTESES a) = evaluateExpr estado a

evaluateExpr estado (CRIAVALOREXPR (CRIAULTVAL (VALOR p) var)) = 
    case val of
        ValorPonteiro s -> (getValor $ getVariavel s estado1, getTipoApontado tipo, estado1)
        otherwise -> error $ "Busca por valor em variável que não é um ponteiro: posição: " ++ (show p) ++ ", tipo: " ++ (show tipo)
    where 
        (val, tipo, estado1) = evaluateExpr estado (CRIAVAR var)
        getValor :: (Either ErroEstado Variavel) -> Valor
        getValor (Left _) = error $ "Ponteiro aponta para posição inválida: posição: " ++ (show p) ++ ", tipo: " ++ (show tipo)
        getValor (Right (_,_,val)) = val
        getTipoApontado :: Tipo -> Tipo
        getTipoApontado (TipoPonteiroFim s) = TipoAtomico s
        getTipoApontado (TipoPonteiroRecursivo s) = s
        getTipoApontado _ = error $ "Erro impossível de ocorrer"

evaluateExpr estado (CRIAVALOREXPR (CRIASEQVAL (VALOR p) seqval)) = 
    case val of
        ValorPonteiro s -> (getValor $ getVariavel s estado1, getTipoApontado tipo, estado1)
        otherwise -> error $ "Busca por valor em variável que não é um ponteiro: posição: " ++ (show p) ++ ", tipo: " ++ (show tipo)
    where 
        (val, tipo, estado1) = evaluateExpr estado (CRIAVALOREXPR seqval)
        getValor :: (Either ErroEstado Variavel) -> Valor
        getValor (Left _) = error $ "Ponteiro aponta para posição inválida: posição: " ++ (show p) ++ ", tipo: " ++ (show tipo)
        getValor (Right (_,_,val)) = val
        getTipoApontado :: Tipo -> Tipo
        getTipoApontado (TipoPonteiroFim s) = TipoAtomico s
        getTipoApontado (TipoPonteiroRecursivo s) = s
        getTipoApontado _ = error $ "Erro impossível de ocorrer"

-- variável simples
evaluateExpr estado (CRIAVAR (Var [SingleVar (ID posicao nome) (OptionalSQBrack [])])) = 
    case (getVariavel nome estado) of
        Right (_,tipo,valor) -> (valor,tipo,estado)
        Left erro -> error $ (show erro) ++ ": posição " ++ (show posicao)

-- variáveis simples com acesso a vetor
evaluateExpr estado (CRIAVAR (Var [SingleVar (ID posicao nome) (OptionalSQBrack ids)])) =
    case (getVariavel nome estado) of
        Right (_, TipoVetor faixas etc, valor) ->
            case (evaluateVet estado valor (TipoVetor faixas etc) faixas ids) of
                Right result -> result
                Left err     -> error $ err ++ ": posição " ++ (show posicao)
        Right _ -> error $ "Variável " ++ nome ++ " não é um vetor: posição " ++ (show posicao)
        Left erro -> error $ (show erro) ++ ": posição " ++ (show posicao)

-- variáveis com acesso a campo de estrutura
evaluateExpr estado (CRIAVAR (Var ((SingleVar (ID posicao nome) (OptionalSQBrack [])):snglVars))) =
    case (getVariavel nome estado) of
        Right (_, TipoEstrutura nome_estr _, valor_estr) ->
            case evaluateEstr estado valor_estr snglVars of
                Right result -> result
                Left erro -> error $ nome_estr ++ " " ++ (show erro) ++ ": posição " ++ (show posicao)
        Right _ -> error $ "Variável " ++ nome ++ " não é uma estrutura: posição " ++ (show posicao)
        Left erro -> error $ (show erro) ++ ": posição " ++ (show posicao)

-- variáveis vetores de estruturas com acesso aos campos
evaluateExpr estado (CRIAVAR (Var ((SingleVar (ID posicao nome) (OptionalSQBrack ids)):snglVars))) =
    -- procura por nome na tabela de símbolos
    case (getVariavel nome estado) of
        -- Se for um vetor
        Right (_, TipoVetor faixas etc, valor) ->
            -- pega elemento correspondente no vetor
            case (evaluateVet estado valor (TipoVetor faixas etc) faixas ids) of
                -- Se for uma estrutura
                Right (val_estr@(ValorEstrutura _),_,estado_atualizado) -> 
                -- Calcula o valor para outros campos
                    case evaluateEstr estado_atualizado val_estr snglVars of
                        Right result -> result
                        Left err -> error $ err ++ ": posição " ++ (show posicao)
                Right _  -> error $ nome ++ " não é um vetor de estruturas: posição " ++ (show posicao)
                Left err -> error $ err ++ ": posição " ++ (show posicao)
        Right _ -> error $ "Variável " ++ nome ++ " não é um vetor: posição " ++ (show posicao)
        Left erro -> error $ (show erro) ++ ": posição " ++ (show posicao)

-- Avalia uma estrutura
evaluateEstr :: Estado -> Valor -> [SingleVAR] -> Either String (Valor,Tipo,Estado)
-- Avalia uma estrutura para um campo de endereçamento. Ex.: a.b
evaluateEstr estado (ValorEstrutura vars_estr) [SingleVar (ID posicao nome) (OptionalSQBrack [])] =
    -- procura pela campo 'nome' na lista de campos da estrutura
    case (getVariavelTabela nome vars_estr) of
        Just (_,tipo, valor) -> Right (valor,tipo,estado)
        Nothing -> Left $ "não possui o campo " ++ nome

-- Avalia um vetor de estruturas com acesso a um campo de endereçamento Ex.: vet[10,10].b
evaluateEstr estado (ValorEstrutura vars_estr) [SingleVar (ID posicao nome) (OptionalSQBrack exprs@(_:_))] =
    --- procura pela campo 'nome' na lista de campos da estrutura
    case (getVariavelTabela nome vars_estr) of
        -- Se nome for um vetor
        Just (_,TipoVetor dims etc, valor) ->
            -- procura o elemento correspondente no vetor
            case evaluateVet estado valor (TipoVetor dims etc) dims exprs of
                Right result -> Right result
                Left err -> error $ err ++ ": posição " ++ (show posicao)
        Just _ -> error $ "Variável " ++ nome ++ " não é um vetor: posição " ++ (show posicao)
        Nothing -> Left $ "não possui o campo " ++ nome

-- Avalia uma estrutura para vários acessos à campo. Ex.: a.first.b.k.p
evaluateEstr estado (ValorEstrutura vars_estr) ((SingleVar (ID posicao nome) (OptionalSQBrack [])):snglVars) = 
    -- procura o primeiro campo 'nome' na lista de campos da estrutura
    case (getVariavelTabela nome vars_estr) of
        -- Se for outra esrtrutura, procura para o resto dos campos
        Just (_, TipoEstrutura _ _, valor_estr) ->  evaluateEstr estado valor_estr snglVars
        Just _ -> error $ nome ++ " não é uma estrutura: posição " ++ (show posicao)
        Nothing -> Left $ "não possui o campo " ++ nome

-- Avalia um vetor de estruturas com vários acessos a campo. Ex.: vet[10].a.b.c
evaluateEstr estado (ValorEstrutura vars_estr) ((SingleVar (ID posicao nome) (OptionalSQBrack exprs@(_:_))):snglVars) = 
    -- procura no escopo pelo variável nome
    case (getVariavelTabela nome vars_estr) of
        -- Se nome for um vetor
        Just (_, TipoVetor dims etc, valor_vet) ->
            -- pega o elemento correspondente no vetor
            case evaluateVet estado valor_vet (TipoVetor dims etc) dims exprs of
                -- Se tal elemento for uma estrutura, procura para o resto dos campos
                Right (val_estr@(ValorEstrutura _),_,estado_atualizado) -> evaluateEstr estado_atualizado val_estr snglVars
                Right _ -> error $ nome ++ " não é uma estrutura: posição " ++ (show posicao)
                Left err -> error $ err ++ ": posição " ++ (show posicao)
        Just _ -> error $ nome ++ " não é uma vetor: posição " ++ (show posicao)
        Nothing -> Left $ "não possui o campo " ++ nome
{-
    estado
    vetor
    limites
    indices

    retorna (Valor,Estado)
-}
evaluateVet :: Estado -> Valor  -> Tipo -> [Integer] -> [EXPR] -> Either String (Valor,Tipo,Estado)
evaluateVet _ _ _ [] [expr] = Left "O número de índices é maior que o número de dimensões no vetor"
evaluateVet _ _ _ [dim] [] = Left "O número de índices é menor que o número de dimensões no vetor"
evaluateVet estado (ValorVetor valores) (TipoVetor _ tipo) [dim] [expr] = 
    case res_expr of
        (ValorInteiro i, _, estado_atualizado) ->
            case getIth valores i of
                Right valor -> Right (valor, tipo, estado_atualizado)
                Left err    -> Left err
        (ValorLogico _, _, _)    -> Left "Valor LOGICO passado como subscrito de vetor"
        (ValorTexto _, _, _)     -> Left "Valor TEXTO passado como subscrito de vetor"
        (ValorCaractere _, _, _) -> Left "Valor CARACTERE passado como subscrito de vetor"
        (ValorReal _, _, _)      -> Left "Valor REAL passado como subscrito de vetor"
        (ValorVetor _, _, _)     -> Left "vetor passado como subscrito de outro vetor"
        (ValorPonteiro _, p, _)  -> Left $ (show p) ++ " passado como subscrito de outro vetor"
        (ValorEstrutura _, _, _) -> Left "ESTRUTURA passada como subscrito de vetor"
    where
        res_expr = evaluateExpr estado expr

evaluateVet estado (ValorVetor valores) tipoVetor (dim:dims) (expr:exprs) = 
    case res_expr of
        (ValorInteiro i, _, estado_atualizado) ->
            case getIth valores i of
                Right valor -> evaluateVet estado_atualizado valor tipoVetor dims exprs 
                Left err    -> Left err
        (ValorLogico _, _, _)    -> Left "Valor LOGICO passado como subscrito de vetor"
        (ValorTexto _, _, _)     -> Left "Valor TEXTO passado como subscrito de vetor"
        (ValorCaractere _, _, _) -> Left "Valor CARACTERE passado como subscrito de vetor"
        (ValorReal _, _, _)      -> Left "Valor REAL passado como subscrito de vetor"
        (ValorVetor _, _, _)     -> Left "vetor passado como subscrito de outro vetor"
        (ValorPonteiro _, p, _)  -> Left $ (show p) ++ " passado como subscrito de outro vetor"
        (ValorEstrutura _, _, _) -> Left "ESTRUTURA passada como subscrito de vetor"
    where
        res_expr = evaluateExpr estado expr

getIth :: [t] -> Integer -> Either String t
getIth [] _ = Left "Indice fora de faixa"
getIth (a:b) i
    | i < 1     = Left "Indice fora de faixa" 
    | i == 1    = Right a
    | otherwise = getIth b (i-1)

{-

    CRIAVAR VAR |
    CRIACHAMADAFUNC CHAMADA |
    CRIANOVO [PONT] {-TIPO-}Token OptionalSQBRACK |
-}
