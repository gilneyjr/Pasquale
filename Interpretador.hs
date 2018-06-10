module Interpretador where

import Data.Fixed
import Data.List
import Data.Maybe
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
    estado1 <- addEstrs estrs inicializarPrograma
    estado2 <- addDecs decs estado1
    estado3 <- addSubprogs subprogs estado2
    --rodaMain main estado3
    print estado3
    return ()

inicializarPrograma :: Estado
inicializarPrograma = criarEscopo 0 estadoinicial

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
getTipoFromEstr (NOVOESTR (TIPO _ nome) decs) estado = TipoEstrutura nome (getDecsEstr nome decs estado)  

--Retorna as declarações de uma estrutura
getDecsEstr :: String -> [DEC] -> Estado -> [Declaracao]
getDecsEstr _ [] _ = []
getDecsEstr nomeEstrutura ((NOVADEC ponteiros (TIPO posicao nome) tokensVariaveis):declaracoes) estado =
    case tipoPrimitivo of
        Right tipoEncontrado -> (zip variaveis (map (getTipoPonteiro ponteiros) (f tipoEncontrado))) ++ (getDecsEstr nomeEstrutura declaracoes estado)
        Left erro -> 
            if nomeEstrutura == nome then
                (zip variaveis (map (getTipoPonteiro ponteiros) (f (TipoEstrutura nome [])))) ++ (getDecsEstr nomeEstrutura declaracoes estado)
            else
                fail $ (show erro) ++ ": posição " ++ (show posicao)
    where tipoPrimitivo = getTipo nome estado
          f tipo = map (\token -> getTipoVetor tipo token estado) tokensVariaveis
          variaveis = map getNomeVar tokensVariaveis

getDecs :: [DEC] -> Estado -> [Declaracao]
getDecs [] _ = []
getDecs ((NOVADEC ponteiros (TIPO posicao nome) tokensVariaveis):declaracoes) estado =
    case tipoPrimitivo of
        Right tipoEncontrado -> (zip variaveis (map (getTipoPonteiro ponteiros) (f tipoEncontrado))) ++ (getDecs declaracoes estado)
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where tipoPrimitivo = getTipo nome estado
          f tipo = map (\token -> getTipoVetor tipo token estado) tokensVariaveis
          variaveis = map getNomeVar tokensVariaveis

getTipoPonteiro :: [PONT] -> Tipo -> Tipo
getTipoPonteiro [] tipo = tipo
getTipoPonteiro [pont] (TipoAtomico nome) = TipoPonteiroFim nome
getTipoPonteiro [pont] (TipoEstrutura nome _) = TipoPonteiroFim nome
getTipoPonteiro (pont:ponts) tipo = TipoPonteiroRecursivo $ getTipoPonteiro ponts tipo

getTipoVetor :: Tipo -> VAR_ -> Estado -> Tipo
getTipoVetor tipo (VAR_SEM (SingleVar _ (OptionalSQBrack []))) estado = tipo
getTipoVetor tipo (VAR_COM (CRIAATRIB (SingleVar _ (OptionalSQBrack [])) _)) _ = tipo
getTipoVetor tipo (VAR_SEM (SingleVar posicao (OptionalSQBrack exprs))) estado =
    if all (\v -> isJust v) valores then
        TipoVetor (catMaybes valores) tipo
    else
        error $ "Expressão não é um valor inteiro valido: posição: " ++ (show posicao)
    where valores = map (\e -> getValorInteiro (evaluateExpr estado e)) exprs

getTipoVetor tipo (VAR_COM (CRIAATRIB (SingleVar posicao (OptionalSQBrack exprs)) _)) estado =
    if all (\v -> isJust v) valores then
        TipoVetor (catMaybes valores) tipo
    else
        error $ "Expressão não é um valor inteiro valido: posição: " ++ (show posicao)
    where valores = map (\e -> getValorInteiro (evaluateExpr estado e)) exprs

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
addDec (NOVADEC _ _ []) estado = do return estado

addDec declaracao@(NOVADEC pont tipo ((VAR_SEM id):b)) estado =
    case res of
        Right estadoAtualizado -> addDec (NOVADEC pont tipo b) estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where (nome', tipo') = head $ getDecs [declaracao] estado
          res = addVariavel (nome', tipo', getValorInicial tipo') estado
          posicao = getPosicaoSingleVar id

addDec declaracao@(NOVADEC pont tipo ((VAR_COM (CRIAATRIB id expr)):b)) estado =
    case res of
        Right estadoAtualizado -> addDec (NOVADEC pont tipo b) estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where (nome', tipo') = head $ getDecs [declaracao] estado
          res = addVariavel (nome', tipo', evaluateExpr estado expr) estado
          posicao = getPosicaoSingleVar id

--Retorna a posicao em que esta o nome de uma variavel
getPosicaoSingleVar :: SingleVAR -> (Int,Int)
getPosicaoSingleVar (SingleVar (ID a _) _) = a

--adiciona os subprogramas criadas pelo usuario
addSubprogs :: [SUBPROG] -> Estado -> IO Estado
addSubprogs []    estado = do return estado
addSubprogs ((CRIAFUNC func):b) estado =
    case novo of
        Right estadoAtualizado -> addSubprogs b estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where novo = addSubprograma (getSubprogFromFunc func estado) estado
          posicao = getPosicaoFunc func
addSubprogs ((CRIAPROC proc):b) estado =
    case novo of
        Right estadoAtualizado -> addSubprogs b estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where novo = addSubprograma (getSubprogFromProc proc estado) estado
          posicao = getPosicaoProc proc
addSubprogs ((CRIAOPER oper):b) estado =
    case novo of
        Right estadoAtualizado -> addSubprogs b estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where novo = addSubprograma (getSubprogFromOper oper estado) estado
          posicao = getPosicaoOper oper

--Retorna o subprograma a ser salvo na memoria
getSubprogFromFunc :: FUNC -> Estado -> Subprograma
getSubprogFromFunc (NOVOFUNC (ID p s) params ponts tipo stmts) estado = 
    Right (s, getDecsFromParams params estado, stmts, getTipoFromTipoRetorno ponts tipo estado)

--retorna a posicao da declaracao de uma funcao
getPosicaoFunc :: FUNC -> (Int,Int)
getPosicaoFunc (NOVOFUNC (ID p _) _ _ _ _) = p


--Retorna o subprograma a ser salvo na memoria
getSubprogFromProc :: PROC -> Estado -> Subprograma
getSubprogFromProc (NOVOPROC (ID p s) params stmts) estado =
    Left (s, getDecsFromParams params estado, stmts)

--retorna a posicao da declaracao de um procedimento
getPosicaoProc :: PROC -> (Int,Int)
getPosicaoProc (NOVOPROC (ID p _) _ _) = p


--Retorna o subprograma a ser salvo na memoria
getSubprogFromOper :: OPER -> Estado -> Subprograma
getSubprogFromOper (NOVOOPER op params ponts tipo stmts) estado =
    Right (getNomeFromOp op, getDecsFromParams params estado, stmts, getTipoFromTipoRetorno ponts tipo estado)
    
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
getDecsFromParams :: [PARAM] -> Estado -> [Declaracao]
getDecsFromParams [] _ = []
getDecsFromParams parametros estado = getDecs (map paramToDec parametros) estado

paramToDec :: PARAM -> DEC
paramToDec (NOVOPARAM ponts tokenTipo variavel) = NOVADEC ponts tokenTipo [(VAR_SEM variavel)]

--Constroi o tipo de retorno da funcao a partir dos tokens modificadores e de retorno
getTipoFromTipoRetorno :: [PONT] -> Token -> Estado -> Tipo
getTipoFromTipoRetorno ponteiros (TIPO posicao nome) estado = 
    case tipoPrimitivo of
        Right tipoEncontrado -> (getTipoPonteiro ponteiros tipoEncontrado)
        Left erro -> error $ (show erro) ++ ": posição " ++ (show posicao)
    where tipoPrimitivo = getTipo nome estado

--Avalia uma expressao e retorna seu valor
evaluateExpr :: Estado -> EXPR -> Valor
evaluateExpr _ _ = ValorInteiro 0 --MUDAR (TEMPORARIO)