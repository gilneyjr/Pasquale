module Expressoes where

import Arvore
import Tipos

evaluateExpr :: EXPR -> Valor

evaluateExpr (CRIAMULT expr1 expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> a * b
                otherwise -> fail $ "Tipos inválidos para o operador * : posição " -- Mostrar posição aqui
        ValorReal a ->
            case res2 of
                ValorReal b -> a * b
                otherwise -> fail $ "Tipos inválidos para o operador * : posição " -- Mostrar posição aqui
        otherwise -> fail $ "Tipos inválidos para o operador * : posição " -- Mostrar posição aqui
    where
        res1 = evaluateExpr expr1
        res2 = evaluateExpr expr2


evaluateExpr (CRIATEXTO (TEXTO _ t)) = ValorTexto t -- OK
evaluateExpr (CRIAINT (INTEIRO _ i)) = ValorInteiro i -- OK
evaluateExpr (CRIACARACTERE (CARACTERE c)) = ValorCaractere c -- OK
evaluateExpr (CRIALOGICO (LOGICO l)) = ValorLogico l -- OK
evaluateExpr (CRIAREAL (REAL _ r)) = ValorReal r -- OK
evaluateExpr (CRIAPARENTESES a) = evaluateExpr a -- OK
