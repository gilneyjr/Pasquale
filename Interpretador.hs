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
    estado3 <- addSubprogs subprogs estado2
    --rodaMain main estado3
    return ()

--adiciona as estruturas criadas pelo usuario
addEstrs :: [ESTR] -> Estado -> IO Estado
addEstrs []    estado = return estado
addEstrs (a:b) estado =
    case novo of
        Right estadoAtualizado -> addEstrs b estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where novo = addTipo (getTipoFromEstr a estado) estado
          posicao = getPosicaoEstr a

--Retorna o tipo de uma estrutura
getTipoFromEstr :: ESTR -> Estado -> Tipo
getTipoFromEstr (NOVOESTR (TIPO _ nome) decs) estado = TipoEstrutura nome (getDecEstr nome decs estado)  

--Retorna as declarações de uma estrutura
getDecEstr :: String -> [DEC_ESTR] -> Estado -> [Declaracao]
getDecEstr _ [] _ = []
getDecEstr nomeEstrutura ((NOVADEC_ESTR ponteiros tokenTipo@(TIPO posicao nome) tokenVariaveis):declaracoes) estado =
    case tipo of
        Right tipoEncontrado -> (zip variaveis (repeat (getTipoPonteiro ponteiros tipoEncontrado))) ++ (getDecEstr nomeEstrutura declaracoes estado)
        Left erro -> 
            if nomeEstrutura == nome then
                (zip variaveis (repeat (getTipoPonteiro ponteiros (TipoEstrutura nome [])))) ++ (getDecEstr nomeEstrutura declaracoes estado)
            else
                fail $ (show erro) ++ ": posição " ++ (show posicao)
    where tipo = getTipo nome estado
          variaveis = map getNomeVar tokenVariaveis

getTipoPonteiro :: [PONT] -> Tipo -> Tipo
getTipoPonteiro [] tipo = tipo
getTipoPonteiro [pont] (TipoAtomico nome) = TipoPonteiroFim nome
getTipoPonteiro [pont] (TipoEstrutura nome _) = TipoPonteiroFim nome
getTipoPonteiro (pont:ponts) tipo = TipoPonteiroRecursivo $ getTipoPonteiro ponts tipo

getNomeVar :: VAR_ -> String
getNomeVar (VAR_SEM (SingleVar (ID _ nome) _)) = nome
getNomeVar (VAR_COM (CRIAATRIB (SingleVar (ID _ nome) _) _)) = nome

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
    where res = addVariavel (getNameSingleVar id, (getTipoFromDec (pont,tipo,id,estado)), evaluateExpr expr estado) estado
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

--adiciona os subprogramas criadas pelo usuario
addSubprogs :: [SUBPROG] -> Estado -> IO Estado
addSubprogs []    estado = do return estado
addSubprogs ((CRIAFUNC func):b) estado =
    case novo of
        Right estadoAtualizado -> addSubprogs b estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where novo = addSubprograma (getSubprogFromFunc func) estado
          posicao = getPosicaoFunc func
addSubprogs ((CRIAPROC proc):b) estado =
    case novo of
        Right estadoAtualizado -> addSubprogs b estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where novo = addSubprograma (getSubprogFromProc proc) estado
          posicao = getPosicaoProc proc
addSubprogs ((CRIAOPER oper):b) estado =
    case novo of
        Right estadoAtualizado -> addSubprogs b estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where novo = addSubprograma (getSubprogFromOper oper) estado
          posicao = getPosicaoOper oper

--Retorna o subprograma a ser salvo na memoria
getSubprogFromFunc :: FUNC -> Subprograma
getSubprogFromFunc (NOVOFUNC (ID p s) params ponts tipo stmts) = 
    Right (s, getDecsFromParams params, stmts, getTipoFromTipoRetorno ponts tipo)

--retorna a posicao da declaracao de uma funcao
getPosicaoFunc :: FUNC -> (Int,Int)
getPosicaoFunc (NOVOFUNC (ID p _) _ _ _ _) = p


--Retorna o subprograma a ser salvo na memoria
getSubprogFromProc :: PROC -> Subprograma
getSubprogFromProc (NOVOPROC (ID p s) params stmts) =
    Left (s, getDecsFromParams params, stmts)

--retorna a posicao da declaracao de um procedimento
getPosicaoProc :: PROC -> (Int,Int)
getPosicaoProc (NOVOPROC (ID p _) _ _) = p


--Retorna o subprograma a ser salvo na memoria
getSubprogFromOper :: OPER -> Subprograma
getSubprogFromOper (NOVOOPER op params ponts tipo stmts) =
    Right (getNomeFromOp op, getDecsFromParams params, stmts, getTipoFromTipoRetorno ponts tipo)
    
--retorna a posicao da declaracao de um operador
getPosicaoOper :: OPER -> (Int,Int)
getPosicaoOper (NOVOOPER (NOVOAdd (Add p)) _ _ _ _) = p
getPosicaoOper (NOVOOPER (NOVOSub (Sub p)) _ _ _ _) = p
getPosicaoOper (NOVOOPER (NOVOMult (Mult p)) _ _ _ _) = p
getPosicaoOper (NOVOOPER (NOVODiv (Div p)) _ _ _ _) = p
getPosicaoOper (NOVOOPER (NOVOGeq (Geq p)) _ _ _ _) = p
getPosicaoOper (NOVOOPER (NOVOLeq (Leq p)) _ _ _ _) = p
getPosicaoOper (NOVOOPER (NOVODiff (Diff p)) _ _ _ _) = p
getPosicaoOper (NOVOOPER (NOVOEqual (Equal p)) _ _ _ _) = p
getPosicaoOper (NOVOOPER (NOVOGreat (Great p)) _ _ _ _) = p
getPosicaoOper (NOVOOPER (NOVOLess (Less p)) _ _ _ _) = p

--Retorna a string com o simbolo do operador
getNomeFromOp :: OP -> String
getNomeFromOp (NOVOAdd _) = "+"
getNomeFromOp (NOVOSub _) = "-"
getNomeFromOp (NOVOMult _) = "*"
getNomeFromOp (NOVODiv _) = "/"
getNomeFromOp (NOVOGeq _) = ">="
getNomeFromOp (NOVOLeq _) = "<="
getNomeFromOp (NOVODiff _) = "-"
getNomeFromOp (NOVOEqual _) = "="
getNomeFromOp (NOVOGreat _) = ">"
getNomeFromOp (NOVOLess _) = "<"

--Retorna um vetor com as Declaracoes dos parametros do subprograma
getDecsFromParams :: [PARAM] -> [Declaracao]
getDecsFromParams _ = [] --MUDAR (TEMPORARIO)

--Constroi o tipo de retorno da funcao a partir dos tokens modificadores e de retorno
getTipoFromTipoRetorno :: [PONT] -> Token -> Tipo
getTipoFromTipoRetorno _ _ = TipoAtomico "INTEIRO" --MUDAR (TEMPORARIO)

--Avalia uma expressao e retorna seu valor
evaluateExpr :: EXPR -> Estado -> Valor
evaluateExpr _ _ = ValorInteiro 0 --MUDAR (TEMPORARIO)

{-
REESCREVER, adicionar estado

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


