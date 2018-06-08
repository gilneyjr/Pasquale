module Interpretador where

import Data.Fixed
import Data.List
import System.IO
import Tipos
import Estado
import Lexico
import Arvore

--Estado antes da execucao
estadoinicial = ([], TipoAtomico "INTEIRO":TipoAtomico "REAL":TipoAtomico "LOGICO":TipoAtomico "TEXTO":TipoAtomico "CARACTERE":[], [])

--Funcao para executar a partir da arvore
executaPrograma :: PROGRAMA -> IO()
executaPrograma (CRIAPROG (INICIOESTRS estrs (INICIODECS decs (INICIOFUNCS subprogs main)))) = do
    estado1 <- addEstrs estrs estadoinicial
    estado2 <- addDecs decs estado1
    --estado3 <- addSubprogs subprogs estado2
    --rodaMain main estado3
    return ()

--adiciona as estruturas criadas pelo usuario
addEstrs :: [ESTR] -> Estado -> IO Estado
addEstrs []    estado = do return estado
addEstrs (a:b) estado =
    case novo of
        Right estadoAtualizado -> addEstrs b estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where novo = addTipo (getTipoFromEstr a) estado
          posicao = getPosicaoEstr a

--Retorna o tipo de uma estrutura
getTipoFromEstr :: ESTR -> Tipo
getTipoFromEstr _ = TipoAtomico "INTEIRO" --MUDAR (TEMPORARIO)

--retorna a posicao da declaracao de uma estrutura para caso esteja repetida
getPosicaoEstr :: ESTR -> (Int,Int)
getPosicaoEstr (NOVOESTR (TIPO p _) _) = p

--Adiciona as declaracoes das variaveis globais
addDecs :: [DEC] -> Estado -> IO Estado
addDecs []    estado = do return estado
addDecs (a:b) estado = do
    estadoAtualizado <- (addDec a estado)
    res <- addDecs b estadoAtualizado
    return res

--Adiciona uma declaracao global
addDec :: DEC -> Estado -> IO Estado
addDec (NOVADEC _ _ (CRIAIDS [])) estado = do return estado

addDec (NOVADEC pont tipo (CRIAIDS ((VAR_SEM id):b))) estado =
    case res of
        Right estadoAtualizado -> addDec (NOVADEC pont tipo (CRIAIDS b)) estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where res = addVariavel (getNameSingleVar id, (getTipoFromDec (pont,tipo,id,estado)), getValorInicial (getTipoFromDec (pont,tipo,id,estado))) estado
          posicao = getPosicaoSingleVar id

addDec (NOVADEC pont tipo (CRIAIDS ((VAR_COM (CRIAATRIB id expr)):b))) estado =
    case res of
        Right estadoAtualizado -> addDec (NOVADEC pont tipo (CRIAIDS b)) estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where res = addVariavel (getNameSingleVar id, (getTipoFromDec (pont,tipo,id,estado)), evaluateExpr (expr, estado)) estado
          posicao = getPosicaoSingleVar id

--Retorna o tipo a partir de uma declaração: recebe ([ponteiro], tipo, variavel (talvez com []), estado)
getTipoFromDec :: ([PONT], Token, SingleVAR, Estado) -> Tipo
getTipoFromDec _ = TipoAtomico "INTEIRO" --MUDAR (TEMPORARIO)

--Retorna uma string com o nome da variavel
getNameSingleVar :: SingleVAR -> String
getNameSingleVar (SingleVar (TIPO _ a) _) = a

--Retorna a posicao em que esta o nome de uma variavel
getPosicaoSingleVar :: SingleVAR -> (Int,Int)
getPosicaoSingleVar (SingleVar (TIPO a _) _) = a

--Avalia uma expressao e retorna seu valor
evaluateExpr :: (EXPR, Estado) -> Valor
evaluateExpr _ = ValorInteiro 0 --MUDAR (TEMPORARIO)

{-
codigo talvez inutil feito por victor

getTipoFromDec :: ([PONT], Token) -> Tipo
getTipoFromDec ([], y) = getTipoFromToken y
getTipoFromDec ((_:ponts), y) = TipoPonteiro $ getTipoFromDec (ponts, y)

getDeclaracaoFromDecID :: (SingleVAR, [PONT], Token) -> Declaracao
getDeclaracaoFromDecID (x, y, z) = (getVarName x, aux x getTipoFromDec (y, z))
    where
        aux :: SingleVar -> Declaracao -> Declaracao
        aux (SingleVar _ (OptionalSQBrack [])) x = x
        aux (SingleVar _ (OptionalSQBrack ids)) x = (x, TipoVetor ids )

getDec :: DEC -> [Declaracao]
getDec (NOVADEC x y (CRIAIDS (id:ids))) =
    if null ids then
        [(getDeclaracaoFromDecID (getID id, x, y))]
    else
        (getDeclaracaoFromDecID (getID id, x, y)):(getDec (NOVADEC x y (CRIAIDS ids)))
    where
        getID :: VAR_ -> SingleVAR
        getID (VAR_SEM x) = x
        getID (VAR_COM (CRIAATRIB x _)) = x

getTipoFromEstr (NOVOESTR (TIPO _ x) y) = TipoEstrutura x (getDecs y)
    where
        getDecs :: [DEC] -> [Declaracao]
        getDecs (dec:decs) = 
            if null decs then
                getDec dec
            else
                (getDec dec) ++ (getDecs decs) -}


