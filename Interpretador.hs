module Interpretador where

import Data.Fixed
import Data.List
import Data.Maybe
import Data.Char
import Text.Read
import System.IO
import System.IO.Unsafe
import System.IO.Error
import Control.Exception
import Control.Applicative
import Debug.Trace
import Tipos
import Estado
import Lexico
import Arvore
--import Expressoes

--Estado antes da execucao
estadoinicial = ([], TipoAtomico "INTEIRO":TipoAtomico "REAL":TipoAtomico "LOGICO":TipoAtomico "TEXTO":TipoAtomico "CARACTERE":[], [], 1)

type EstadoCompleto = (Estado, Bool, Bool, Bool, Maybe EXPR, Maybe (Int,Int))

--Funcao para executar a partir da arvore
executaPrograma :: PROGRAMA -> IO()
executaPrograma (CRIAPROG (INICIOESTRS estrs (INICIODECS decs (INICIOFUNCS subprogs main)))) = do
    -- Adiciona declarações de novos tipos estrutura
    estado1 <- addEstrs estrs inicializarPrograma
    -- Adiciona declarações de variáveis globais
    estado2 <- addDecs decs estado1
    -- Adiciona as funções e procedimentos definidos pelo usuário
    estado3 <- addSubprogs subprogs estado2
    -- Inicia a execução do programa
    estado4 <- iniciaBlocoMain main estado3
    return ()

-- Estado inicial do programa
inicializarPrograma :: Estado
inicializarPrograma = criarEscopo 0 estadoinicial

-- Adiciona as estruturas criadas pelo usuario
addEstrs :: [ESTR] -> Estado -> IO Estado
-- Caso não hajam estruturas para adicionar
addEstrs []    estado = return estado
-- Caso hajam estruturas para adicionar
addEstrs (a:b) estado =
    -- Caso tenha adicionado com sucesso, chama recursivamente para o resto das estruturas
    case novo of
        Right estadoAtualizado -> addEstrs b estadoAtualizado
        Left erro -> fail $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where (tipoEstrutura, estadoFinal) = (getTipoFromEstr a estado)
          novo = addTipo tipoEstrutura estadoFinal
          posicao = getPosicaoEstr a

-- Dados a estrutura na árvore e o estado atual, retorna o tipo de uma estrutura
getTipoFromEstr :: ESTR -> Estado -> (Tipo, Estado)
getTipoFromEstr (NOVOESTR (TIPO _ nome) decs) estado = ((TipoEstrutura nome declaracoes), estadoFinal)
    where (declaracoes, estadoFinal) = (getDecsEstr nome decs estado)

-- Dados o nome da estrutura, a lista de declarações dela na árvore e o estado atual, retorna as declarações dessa estrutura
getDecsEstr :: String -> [DEC] -> Estado -> ([Declaracao], Estado)
getDecsEstr _ [] estado = ([], estado)
getDecsEstr nomeEstrutura ((NOVADEC ponteiros (TIPO posicao nome) tokensVariaveis):declaracoes) estado =
    case tipoPrimitivo of
        Right tipoEncontrado -> 
            let (tipos, estadoIntermediario) = f tipoEncontrado
                (declaracoes', estadoFinal) = getDecsEstr nomeEstrutura declaracoes estadoIntermediario in
            ((zip variaveis (map (getTipoPonteiro ponteiros) tipos)) ++ declaracoes', estadoFinal)
        Left erro -> 
            if nomeEstrutura == nome then
                let (tipos, estadoIntermediario) = f (TipoEstrutura nome [])
                    (declaracoes', estadoFinal) = getDecsEstr nomeEstrutura declaracoes estadoIntermediario in
                ((zip variaveis (map (getTipoPonteiro ponteiros) tipos)) ++ declaracoes', estadoFinal)
            else
                error $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where tipoPrimitivo = getTipo nome estado
          f tipo = foldl (funcaoFold' tipo) ([], estado) tokensVariaveis
          variaveis = map getNomeVar tokensVariaveis

funcaoFold' :: Tipo -> ([Tipo], Estado) -> VAR_ -> ([Tipo], Estado)
funcaoFold' tipo (tipos, estadoInicial) token = ((tipos ++ [tipo']), estadoFinal)
    where (tipo', estadoFinal) = (getTipoVetor tipo token estadoInicial)

getDecs :: [DEC] -> Estado -> ([Declaracao], Estado)
getDecs [] estado = ([], estado)
getDecs ((NOVADEC ponteiros (TIPO posicao nome) tokensVariaveis):declaracoes) estado =
    case tipoPrimitivo of
        Right tipoEncontrado -> 
            let (tipos, estadoIntermediario) = foldl (funcaoFold' (getTipoPonteiro ponteiros tipoEncontrado)) ([], estado) tokensVariaveis
                (declaracoes', estadoFinal) = getDecs declaracoes estadoIntermediario in
            (zip variaveis tipos ++ declaracoes', estadoFinal)
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where tipoPrimitivo = getTipo nome estado
          variaveis = map getNomeVar tokensVariaveis

getTipoVetor :: Tipo -> VAR_ -> Estado -> (Tipo, Estado)
getTipoVetor tipo (VAR_SEM (SingleVar _ (OptionalSQBrack []))) estado = (tipo, estado)
getTipoVetor tipo (VAR_COM (CRIAATRIB (SingleVar _ (OptionalSQBrack [])) _)) estado = (tipo, estado)
getTipoVetor tipo (VAR_SEM (SingleVar posicao (OptionalSQBrack exprs))) estado =
    if all isJust valores then
        (TipoVetor (catMaybes valores) tipo, estadoFinal)
    else
        error $ "Expressão não é um valor inteiro valido\nPosição: " ++ (show posicao)
    where (valores, estadoFinal) = foldl funcaoFold ([], estado) exprs

getTipoVetor tipo (VAR_COM (CRIAATRIB (SingleVar posicao (OptionalSQBrack exprs)) _)) estado =
    if all (\v -> isJust v) valores then
        (TipoVetor (catMaybes valores) tipo, estadoFinal)
    else
        error $ "Expressão não é um valor inteiro valido\nPosição: " ++ (show posicao)
    where (valores, estadoFinal) = foldl funcaoFold ([], estado) exprs

funcaoFold :: ([Maybe Integer], Estado) -> EXPR -> ([Maybe Integer], Estado)
funcaoFold (valores, estadoInicial) expr = ((valores ++ [getValorInteiro valor]), estadoFinal)
    where (valor, _, estadoFinal) = evaluateExpr estadoInicial expr

getNomeVar :: VAR_ -> String
getNomeVar (VAR_SEM (SingleVar (ID _ nome) _)) = nome
getNomeVar (VAR_COM (CRIAATRIB (SingleVar (ID _ nome) _) _)) = nome

--retorna a posicao da declaracao de uma estrutura para caso esteja repetida
getPosicaoEstr :: ESTR -> (Int,Int)
getPosicaoEstr (NOVOESTR (TIPO p _) _) = p

--Adiciona as declaracoes das variaveis globais
addDecs :: [DEC] -> Estado -> IO Estado
addDecs []    estado = do return estado
addDecs (a:b) estado = (addDec a estado) >>= (addDecs b)

--Adiciona uma declaracao global
addDec :: DEC -> Estado -> IO Estado
addDec (NOVADEC _ _ []) estado = do return estado

addDec declaracao@(NOVADEC pont tipo ((VAR_SEM id):b)) estado =
    case res of
        Right estadoAtualizado -> addDec (NOVADEC pont tipo b) estadoAtualizado
        Left erro -> fail $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where ((nome', tipo'):_, estadoIntermediario) = getDecs [(NOVADEC pont tipo [(VAR_SEM id)])] estado
          res = addVariavel (nome', tipo', getValorInicial tipo') estadoIntermediario
          posicao = getPosicaoSingleVar id

addDec declaracao@(NOVADEC pont tipo ((VAR_COM (CRIAATRIB id expr)):b)) estado =
    case res of
        Right estadoAtualizado -> addDec (NOVADEC pont tipo b) estadoAtualizado
        Left erro -> fail $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where ((nome', tipo'):_, estadoIntermediario) = getDecs [(NOVADEC pont tipo [(VAR_COM (CRIAATRIB id expr))])] estado
          (valor, _, estado') = evaluateExpr estadoIntermediario expr
          res = addVariavel (nome', tipo', valor) estado'
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
        Left erro -> fail $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where (subprograma, estadoIntermediario) = getSubprogFromFunc func estado
          novo = addSubprograma subprograma estadoIntermediario
          posicao = getPosicaoFunc func
addSubprogs ((CRIAPROC proc):b) estado =
    case novo of
        Right estadoAtualizado -> addSubprogs b estadoAtualizado
        Left erro -> fail $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where (subprograma, estadoIntermediario) = getSubprogFromProc proc estado
          novo = addSubprograma subprograma estadoIntermediario
          posicao = getPosicaoProc proc
addSubprogs ((CRIAOPER oper):b) estado =
    case novo of
        Right estadoAtualizado -> addSubprogs b estadoAtualizado
        Left erro -> fail $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where (subprograma, estadoIntermediario) = getSubprogFromOper oper estado
          novo = addSubprograma subprograma estadoIntermediario
          posicao = getPosicaoOper oper

--Retorna o subprograma a ser salvo na memoria
getSubprogFromFunc :: FUNC -> Estado -> (Subprograma, Estado)
getSubprogFromFunc (NOVOFUNC (ID p s) params ponts tipo stmts) estado = 
    (Right (s, decs, stmts, getTipoFromTipoRetorno ponts tipo estadoFinal), estadoFinal)
    where (decs, estadoFinal) = getDecsFromParams params estado

--retorna a posicao da declaracao de uma funcao
getPosicaoFunc :: FUNC -> (Int,Int)
getPosicaoFunc (NOVOFUNC (ID p _) _ _ _ _) = p


--Retorna o subprograma a ser salvo na memoria
getSubprogFromProc :: PROC -> Estado -> (Subprograma, Estado)
getSubprogFromProc (NOVOPROC (ID p s) params stmts) estado =
    (Left (s, decs, stmts), estadoFinal)
    where (decs, estadoFinal) = getDecsFromParams params estado

--retorna a posicao da declaracao de um procedimento
getPosicaoProc :: PROC -> (Int,Int)
getPosicaoProc (NOVOPROC (ID p _) _ _) = p


--Retorna o subprograma a ser salvo na memoria
getSubprogFromOper :: OPER -> Estado -> (Subprograma, Estado)
getSubprogFromOper (NOVOOPER op params ponts tipo stmts) estado =
    (Right (getNomeFromOp op, decs, stmts, getTipoFromTipoRetorno ponts tipo estadoFinal), estadoFinal)
    where (decs, estadoFinal) = getDecsFromParams params estado
    
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
getDecsFromParams :: [PARAM] -> Estado -> ([Declaracao], Estado)
getDecsFromParams [] estado = ([], estado)
getDecsFromParams parametros estado = getDecs (map paramToDec parametros) estado

paramToDec :: PARAM -> DEC
paramToDec (NOVOPARAM ponts tokenTipo variavel) = NOVADEC ponts tokenTipo [(VAR_SEM variavel)]

--Constroi o tipo de retorno da funcao a partir dos tokens modificadores e de retorno
getTipoFromTipoRetorno :: [PONT] -> Token -> Estado -> Tipo
getTipoFromTipoRetorno ponteiros (TIPO posicao nome) estado = 
    case tipoPrimitivo of
        Right tipoEncontrado -> (getTipoPonteiro ponteiros tipoEncontrado)
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where tipoPrimitivo = getTipo nome estado

--Insere o escopo da Main, executa, e depois remove
iniciaBlocoMain :: MAIN -> Estado -> IO EstadoCompleto
iniciaBlocoMain (Main stmts) estado = do
    (estado1, temRetorno, temSaia, temContinue, maybeExpr, maybePos) <- rodaStmts stmts (criarEscopo 1 estado)
    case (temRetorno, temSaia, temContinue) of
        (_, False, False) -> 
            case maybeExpr of
                Nothing -> return (removerEscopo estado1, temRetorno, temSaia, temContinue, maybeExpr, maybePos)
                otherwise -> error $ "Erro retorno não vazio\nPosição: " ++ (show $ fromJust maybePos) 
        otherwise -> case maybePos of
            Just p -> error $ "Erro com comando na posição: " ++ show p

--Insere o escopo do Se, executa, e depois remove
iniciaBlocoSe :: [STMT] -> Estado -> IO EstadoCompleto
iniciaBlocoSe stmts estado = do
    (estado1, a, b, c, d, e) <- rodaStmts stmts (criarEscopo (getIdEscopoAtual estado) estado)
    return (removerEscopo estado1, a, b, c, d, e)

--Insere o escopo do Bloco, executa, e depois remove
iniciaBloco :: [STMT] -> Estado -> IO EstadoCompleto
iniciaBloco stmts estado = do
    (estado1, a, b, c, d, e) <- rodaStmts stmts (criarEscopo (getIdEscopoAtual estado) estado)
    return (removerEscopo estado1, a, b, c, d, e)

--Efetua a execução do ENQUANTO
rodaEnquanto :: Token -> EXPR -> [STMT] -> Estado -> IO EstadoCompleto
rodaEnquanto (ENQUANTO p) expr stmts estado = do
    (val,_,estado1) <- (return (evaluateExpr estado expr))
    case val of
        ValorLogico False -> return (estado1, False, False, False, Nothing, Nothing)
        ValorLogico True -> do
            (estado2, temRetorno, temSaia, _, maybeExpr, maybePos) <- rodaStmts stmts (criarEscopo (getIdEscopoAtual estado) estado)
            estado3 <- (return $ removerEscopo estado2)
            case (temRetorno, temSaia) of
                (False, False) -> rodaEnquanto (ENQUANTO p) expr stmts estado3
                otherwise -> return (estado3, temRetorno, False, False, maybeExpr, maybePos)
        otherwise -> error $ "Tipo da expressão não LOGICO na condição do ENQUANTO\nPosição: " ++ show p

--Executa lista de stmts
rodaStmts :: [STMT] -> Estado -> IO EstadoCompleto
rodaStmts [] estado = return (estado, False, False, False, Nothing, Nothing)
rodaStmts ((stmt:stmts)) estado = do
    (estado1, temRetorno, temSaia, temContinue, maybeExpr, maybePos) <- (executarStmt stmt estado)
    case (temRetorno, temSaia, temContinue) of
        (False, False, False) -> rodaStmts stmts estado1
        otherwise -> return (estado1, temRetorno, temSaia, temContinue, maybeExpr, maybePos)

--Executa um stmt
executarStmt :: STMT -> Estado -> IO EstadoCompleto
executarStmt (NOVODEC dec) estado = (addDec dec estado) >>= (\estado' -> return (estado', False, False, False, Nothing, Nothing))
executarStmt (NOVOATRIBSTMT expr (Attrib posicao) expr') estado0 =
    case tipo of
        TipoEstrutura _ _ -> case expr of
            CRIAVALOREXPR _ ->
                case atualizarVariavel (nome, tipo, valor) estadoIntermediario  of
                    Right estado' -> return (estado', False, False, False, Nothing, Nothing)
                    Left erro -> fail $ show erro
            otherwise ->
                let (estruturaNova, estadoFinal) = (atualizarEstrutura expr valorAntigo valor (traduzTipo tipoExpr estadoIntermediario) posicao estadoIntermediario) in
                case atualizarVariavel (nome, tipo, estruturaNova) estadoFinal  of
                    Right estado' -> return (estado', False, False, False, Nothing, Nothing)
                    Left erro -> fail $ show erro
        TipoVetor dimensoes _ ->
            let (vetorNovo, estadoFinal) = (atualizarVetor expr (traduzTipo tipo estado) dimensoes valorAntigo valor (traduzTipo tipoExpr estado) posicao estadoIntermediario) in
            case atualizarVariavel (nome, tipo, vetorNovo) estadoFinal  of
                Right estado' -> return (estado', False, False, False, Nothing, Nothing)
                Left erro -> fail $ show erro
        otherwise ->
            case atualizarVariavel (nome, tipo, valor) estadoIntermediario  of
                Right estado' -> return (estado', False, False, False, Nothing, Nothing)
                Left erro -> fail $ show erro
    where ((nome, tipo, valorAntigo), estado) = getVariavelFromExpr expr estado0
          (valor, tipoExpr, estadoIntermediario) = (evaluateExpr estado expr')

executarStmt (NOVOINC (CRIAINC (Var (tokenNome@(SingleVar (ID p nomeCampo) _):campos)))) estado =
    case getVariavel nomeCampo estado of
        Right (nome, tipo, valor) ->
            if null campos then
                case getValorInteiro valor of
                    Just valor' ->
                        case atualizarVariavel (nome, tipo, ValorInteiro (succ valor')) estado of
                            Right estado' -> do
                                return (estado', False, False, False, Nothing, Nothing)
                            Left erro -> fail $ show erro ++ "\nPosição: " ++ (show p)
                    Nothing -> error $ "Tipo da variável '" ++ nome ++ "' não é INTEIRO\nPosição: " ++ (show p)
            else
                let valorEstrutura = incrementaValorEstrutura campos valor in
                case atualizarVariavel (nome, tipo, valorEstrutura) estado of
                    Right estado' -> return (estado', False, False, False, Nothing, Nothing)
                    Left erro -> fail $ show erro ++ "\nPosição: " ++ (show p)
        Left erro -> fail $ show erro ++ "\nPosição: " ++ (show p)

executarStmt (NOVODECR (CRIADECR (Var (tokenNome@(SingleVar (ID p nomeCampo) _):campos)))) estado = 
    case getVariavel nomeCampo estado of
        Right (nome, tipo, valor) ->
            if null campos then
                case getValorInteiro valor of
                    Just valor' ->
                        case atualizarVariavel (nome, tipo, ValorInteiro (valor' - 1)) estado of
                            Right estado' -> return (estado', False, False, False, Nothing, Nothing)
                            Left erro -> fail $ show erro ++ "\nPosição: " ++ (show p)
                    Nothing -> error $ "Tipo da variável '" ++ nome ++ "' não é INTEIRO\nPosição: " ++ (show p)
            else
                let valorEstrutura = incrementaValorEstrutura campos valor in
                case atualizarVariavel (nome, tipo, valorEstrutura) estado of
                    Right estado' -> return (estado', False, False, False, Nothing, Nothing)
                    Left erro -> fail $ show erro ++ "\nPosição: " ++ (show p)
        Left erro -> fail $ show erro ++ "\nPosição: " ++ (show p)

executarStmt (NOVOCHAMADA (CRIACHAMADA (ID posicao nome) exprs)) estado =
    case subprograma of
        Right (Left (nome, declaracoes, stmts)) ->
            let estadoFinal = foldl funcaoFold'' estadoAtualizado (zip3 (fst $ unzip declaracoes) tiposParametros valoresParametros) in 
            (rodaStmts stmts estadoFinal) >>= (\(estado1, a, b, c, d, e) -> return (removerEscopo estado1, a, b, c, d, e))
        Right (Right (nome, declaracoes, stmts, tipoRetorno)) -> 
            let estadoFinal = foldl funcaoFold'' estadoAtualizado (zip3 (fst $ unzip declaracoes) tiposParametros valoresParametros) in
            (rodaStmts stmts estadoFinal) >>= (\(estado1, a, b, c, d, e) -> return (removerEscopo estado1, a, b, c, d, e))
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where estadoEscopoFuncao = criarEscopo (getIdEscopoAtual estado) estado
          (valoresParametros, tiposParametros, estadoAtualizado) = evaluateExprs estadoEscopoFuncao exprs
          subprograma = getSubprograma nome tiposParametros estadoAtualizado

executarStmt (NOVOSE (CRIASE token expr stmts1 (OptionalSenao stmts2))) estado =
    case res of
        ValorLogico True -> iniciaBlocoSe stmts1 estado1
        ValorLogico False -> iniciaBlocoSe stmts2 estado1
        otherwise -> fail $ "Expressão não é do tipo LOGICO\nPosição: " ++ (show getPosSE)
    where
        (res, _, estado1) = evaluateExpr estado expr
        getPosSE :: Token -> (Int, Int)
        getPosSE (SE p) = p

executarStmt (NOVOENQUANTO (CRIAENQUANTO t expr stmts)) estado =
    rodaEnquanto t expr stmts estado

executarStmt (NOVORETORNEFUNC (CRIARETORNEF (RETORNE p) expr)) estado = 
    return (estado, True, False, False, Just expr, Just p)
    
executarStmt (NOVORETORNEPROC (CRIARETORNEP (RETORNE p))) estado = 
    return (estado, True, False, False, Nothing, Just p)
    
executarStmt (NOVOSAIA (CRIASAIA (SAIA p))) estado = 
    return (estado, False, True, False, Nothing, Just p)
    
executarStmt (NOVOCONTINUE (CRIACONTINUE (CONTINUE p))) estado = 
    return (estado, False, False, True, Nothing, Just p)
    
executarStmt (NOVODELETE (CRIADELETE tok expr)) estado = 
    case val of
        ValorPonteiro s -> case removerVariavel (justVar (getVariavel s estado1)) estado1 of
            Right estado2 -> return (estado2, False, False, False, Nothing, Nothing)
            Left erro -> error $ "Delete em posição da memória inválida: " ++ (show $ getPosFromDelete tok)
        otherwise -> error $ "Operação de delete para expressão que não retorna Ponteiro\nPosição: " ++ (show $ getPosFromDelete tok)
    where
        (val, _, estado1) = evaluateExpr estado expr
        justVar :: (Either ErroEstado Variavel) -> Variavel
        justVar (Right x) = x
        justVar (Left _) = error $ "Delete em posição da memória inválida: " ++ (show $ getPosFromDelete tok)
        getPosFromDelete :: Token -> (Int, Int)
        getPosFromDelete (DELETE p) = p

executarStmt (NOVOESCREVA (CRIAESCREVA (ESCREVA p) expr)) estado =
    case valor1 of
        ValorTexto val -> do
            putStr val
            return (estado1, False, False, False, Nothing, Nothing)
        ValorInteiro val -> do
            putStr $ show val
            return (estado1, False, False, False, Nothing, Nothing)
        ValorReal val -> do
            putStr $ show val
            return (estado1, False, False, False, Nothing, Nothing)
        ValorLogico val -> do
            putStr $ showLogico val
            return (estado1, False, False, False, Nothing, Nothing)
        ValorCaractere val -> do
            putStr $ show val
            return (estado1, False, False, False, Nothing, Nothing)
        otherwise -> error $ "Comando ESCREVA para tipo não primitivo\nPosição: " ++ show p
    where
        (valor1, _, estado1) = evaluateExpr estado expr
        showLogico :: Bool -> String
        showLogico True = "VERDADEIRO"
        showLogico False = "FALSO"

executarStmt (NOVOLEIA (CRIALEIA (LEIA p) [])) estado = return (estado, False, False, False, Nothing, Nothing)
executarStmt (NOVOLEIA (CRIALEIA (LEIA p) (expr:exprs))) estado0 = do
    let ((_, tipo, valor),estado) = getVariavelFromExpr (CRIAVAR expr) estado0
    let tipoAtualizado = case tipo of { TipoAtomico s -> tipo; TipoVetor s tipoPrimitivo -> tipoPrimitivo }
    case tipoAtualizado of
        TipoAtomico "INTEIRO" -> do
            hFlush stdout
            s <- getPalavra
            case readMaybe s :: Maybe Integer of
                Just i -> executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (Attrib p) (CRIAINT (INTEIRO p i))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
                Nothing -> error $ "Valor não permitido como INTEIRO\nPosição: " ++ (show p)
        TipoAtomico "REAL" -> do
            hFlush stdout
            s <- getPalavra
            case readMaybe s :: Maybe Double of
                Just i -> executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (Attrib p) (CRIAREAL (REAL p i))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
                Nothing -> error $ "Valor não permitido como REAL\nPosição: " ++ (show p)
        TipoAtomico "CARACTERE" -> do
            hFlush stdout
            s <- getChar
            executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (Attrib p) (CRIACARACTERE (CARACTERE p s))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
        TipoAtomico "TEXTO" -> do
            hFlush stdout
            s <- getPalavra
            executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (Attrib p) (CRIATEXTO (TEXTO p s))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
        TipoAtomico "LOGICO" -> do
            hFlush stdout
            s <- getPalavra
            case s of
                "VERDADEIRO" -> executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (Attrib p) (CRIALOGICO (LOGICO p True))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
                "FALSO" -> executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (Attrib p) (CRIALOGICO (LOGICO p False))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
                otherwise -> error $ "Valor não permitido como LOGICO\nPosição: " ++ (show p)
        otherwise -> error $ "Comando LEIA para tipo não primitivo\nPosição: " ++ show p

executarStmt (NOVOBLOCO (CRIABLOCO stmts)) estado =
    iniciaBloco stmts estado

getVariavelFromExpr :: EXPR -> Estado -> (Variavel, Estado)
getVariavelFromExpr (CRIAVAR (Var ((SingleVar (ID posicao nome) colchetes):_))) estado = 
    case var of
        Right var' -> (var', estado)
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where var = getVariavel nome estado

getVariavelFromExpr (CRIAVALOREXPR (CRIAULTVAL (VALOR p) var)) estadoAntigo =
    case res of
        ValorPonteiro nome -> case var of
            Right var' -> (var', estado)
            Left erro -> error $ "Ponteiro acessado aponta para posicao invalida\nPosição: " ++ (show p)
            where var = getVariavel nome estado
        otherwise -> error $ "Busca por valor em variável que não é um ponteiro\nPosição: " ++ (show p) ++ ", tipo: " ++ (show tipo)
    where
        (res, tipo, estado) = evaluateExpr estadoAntigo (CRIAVAR var)

getVariavelFromExpr (CRIAVALOREXPR (CRIASEQVAL (VALOR p) val)) estadoAntigo =
    case res of
        ValorPonteiro nome -> case var of
            Right var' -> (var', estado)
            Left erro -> error $ "Ponteiro acessado aponta para posicao invalida\nPosição: " ++ (show p)
            where var = getVariavel nome estado
        otherwise -> error $ "Busca por valor em variável que não é um ponteiro\nPosição: " ++ (show p) ++ ", tipo: " ++ (show tipo)
    where
        (res, tipo, estado) = evaluateExpr estadoAntigo (CRIAVALOREXPR val)

incrementaValorEstrutura :: [SingleVAR] -> Valor -> Valor
incrementaValorEstrutura ((SingleVar (ID p nomeCampo) _):_) (ValorEstrutura []) = error $ "Campo '" ++ nomeCampo ++ "'' não encontrado\nPosição: " ++ (show p)
incrementaValorEstrutura nomes@((SingleVar (ID p nomeCampo) _):nomeCampos) (ValorEstrutura (campo@(nome, tipo, valor):campos)) =
    if nomeCampo == nome then
        if null nomeCampos then
            case getValorInteiro valor of
                Just valor' -> ValorEstrutura ((nome, tipo, ValorInteiro (succ valor')):campos)
                Nothing -> error $ "Tipo da variável '" ++ nome ++ "' não é do tipo INTEIRO\nPosição: " ++ (show p)
        else
            ValorEstrutura ((nome, tipo, (incrementaValorEstrutura nomeCampos valor)):campos)
    else
        ValorEstrutura (campo:camposAtualizado)
    where (ValorEstrutura camposAtualizado) = incrementaValorEstrutura nomes (ValorEstrutura campos)

decrementaValorEstrutura :: [SingleVAR] -> Valor -> Valor
decrementaValorEstrutura ((SingleVar (ID p nomeCampo) _):_) (ValorEstrutura []) = error $ "Campo '" ++ nomeCampo ++ "'' não encontrado posição: " ++ (show p)
decrementaValorEstrutura nomes@((SingleVar (ID p nomeCampo) _):nomeCampos) (ValorEstrutura (campo@(nome, tipo, valor):campos)) =
    if nomeCampo == nome then
        if null nomeCampos then
            case getValorInteiro valor of
                Just valor' -> ValorEstrutura ((nome, tipo, ValorInteiro (valor' - 1)):campos)
                Nothing -> error $ "Tipo da variável '" ++ nome ++ "' não é do tipo INTEIRO\nPosição: " ++ (show p)
        else
            ValorEstrutura ((nome, tipo, (incrementaValorEstrutura nomeCampos valor)):campos)
    else
        ValorEstrutura (campo:camposAtualizado)
    where (ValorEstrutura camposAtualizado) = incrementaValorEstrutura nomes (ValorEstrutura campos)

{-
Atualiza uma posição do vetor:
expressão nome
tipo da expressão nome
dimensões
valor antigo do vetor
valor a ser inserido na posição
tipo do valor a ser inserido
posicao do erro
-}

atualizarVetor :: EXPR -> Tipo -> [Integer] -> Valor -> Valor -> Tipo -> (Int,Int) -> Estado -> (Valor, Estado)
atualizarVetor (CRIAVAR (Var ((SingleVar _ (OptionalSQBrack exprs)):_))) tipoVetor dimensoes (ValorVetor valoresAntigos) valorNovo tipoNovo posicao estado =
    if all isJust posicoes then
        ((ValorVetor (atualizarVetor' valoresAntigos tipoVetor dimensoes (catMaybes posicoes) valorNovo tipoNovo posicao)), estadoAtualizado)
    else
        error $ "Expressão não é um valor inteiro valido"
    where (posicoes, estadoAtualizado) = foldl funcaoFold ([], estado) exprs

--Recebe um vetor, o tipo dele, as dimensoes, as posições, o valor novo, o tipo novo, posicao do erro
atualizarVetor' :: [Valor] -> Tipo -> [Integer] -> [Integer] -> Valor -> Tipo -> (Int,Int) -> [Valor]
atualizarVetor' _ _ [] _ _ _ pos =
    error $ "Muitos subscritos no acesso ao arranjo\nPosição: " ++ (show pos)

atualizarVetor' valoresVetor (TipoVetor _ tipoElem) (dimensao:dimensoes) [posicao] valorNovo tipoNovo pos =
    if (posicao >= 1) && (posicao <= dimensao) then
        if dimensoes == [] then
            if tipoElem == tipoNovo then
                   inicio ++ [valorNovo] ++ (tail fim)
            else
                error $ "Atribuição inválida!\nTipo esperado: " ++ (show tipoElem) ++ "\nTipo recebido: " ++ (show tipoNovo) ++ "\nPosição: " ++ (show pos)
        else
            error $ "Poucos subscritos no acesso ao arranjo\nPosição: " ++ (show pos)
    else
        error $ "Segmentation fault!\nPosição: " ++ show pos
    where (inicio, fim) = genericSplitAt (posicao - 1) valoresVetor

atualizarVetor' valoresVetor tipoVetor (dimensao:dimensoes) (posicao:posicoes) valorNovo tipoNovo pos = 
    if (posicao >= 1) && (posicao <= dimensao) then
        (inicio ++ [ValorVetor (atualizarVetor' valoresAntigos tipoVetor dimensoes posicoes valorNovo tipoNovo pos)] ++ fim)
    else
        error $ "Segmentation fault!\nPosição: " ++ show pos
    where (inicio, meio) = genericSplitAt (posicao - 1) valoresVetor
          ((ValorVetor valoresAntigos):fim) = meio

{-
Atualiza uma posição da estrutura:
expressão nome
declarações
valor antigo da estrutura
valor a ser inserido na posição
tipo do valor a ser inserido
posicao do erro
-}
atualizarEstrutura :: EXPR -> Valor -> Valor -> Tipo -> (Int,Int) -> Estado -> (Valor, Estado)
atualizarEstrutura (CRIAVAR (Var (_:elementos))) (ValorEstrutura variaveisAntigas) valorNovo tipoNovo posicao estado =
    if null elementos then
        (valorNovo,estado)
    else
        ((ValorEstrutura valor),estadoFinal)
    where (valor, estadoFinal) = atualizarEstrutura' elementos variaveisAntigas valorNovo tipoNovo posicao estado

atualizarEstrutura' :: [SingleVAR] -> [Variavel] -> Valor -> Tipo -> (Int,Int) -> Estado -> ([Variavel],Estado)
atualizarEstrutura' _ [] _ _ pos _ =
    error $ "Muitos subscritos no acesso à estrutura\nPosição: " ++ (show pos)

atualizarEstrutura' [variavel@(SingleVar (ID posicao nome) (OptionalSQBrack expr))] variaveisAntigas valorNovo tipoNovo pos estado =
    case var of
        Just (nomeVar, tipoVar, valor) -> 
            if null expr then
                if tipoVar == tipoNovo then
                    ((inicio ++ [(nomeVar, tipoVar, valorNovo)] ++ (tail fim)), estado)
                else error $ "Atribuição inválida!\nTipo esperado: " ++ (show tipoVar) ++ "\nTipo recebido: " ++ (show tipoNovo) ++ "\nPosição: " ++ (show pos)
            else
                let (TipoVetor dimensoes _) = tipoVar
                    (valorVetor, estadoFinal) = (atualizarVetor (CRIAVAR (Var [variavel])) (traduzTipo tipoVar estado) dimensoes valor valorNovo (traduzTipo tipoNovo estado) pos estado) in
                ((inicio ++ [(nomeVar, tipoVar, valorVetor)] ++ (tail fim)), estadoFinal)
        Nothing -> error $ "Campo '" ++ nome ++ "' não definido na estrutura\nPosição: " ++ (show pos)
    where (var, index)  = getVarFromNome nome variaveisAntigas
          (inicio, fim) = genericSplitAt index variaveisAntigas

atualizarEstrutura' (variavel@(SingleVar (ID posicao nome) (OptionalSQBrack expr)):variaveis) variaveisAntigas valorNovo tipoNovo pos estado =
    case var of
        Just (nomeVar, tipoVar, valor@(ValorEstrutura variaveisAntigas')) -> 
            let (valorEstruturaNovo, estadoFinal) = atualizarEstrutura' variaveis variaveisAntigas' valorNovo tipoNovo pos estado in 
            if null expr then
                ((inicio ++ [(nomeVar, tipoVar, (ValorEstrutura valorEstruturaNovo))] ++ (tail fim)), estadoFinal)
            else
                let (TipoVetor dimensoes _) = tipoVar 
                    (valorVetor, estadoIntermediario) = atualizarVetor (CRIAVAR (Var [variavel])) (traduzTipo tipoVar estado) dimensoes valor valorNovo (traduzTipo tipoNovo estado) pos estado in
                ((inicio ++ [(nomeVar, tipoVar, valorVetor)] ++ (tail fim)), estadoIntermediario)
        Nothing -> error $ "Campo '" ++ nome ++ "' não definido na estrutura\nPosição: " ++ (show pos)
    where (var, index)  = getVarFromNome nome variaveisAntigas
          (inicio, fim) = genericSplitAt index variaveisAntigas

getVarFromNome :: String -> [Variavel] -> (Maybe Variavel,Int)
getVarFromNome _ [] = (Nothing,-1)
getVarFromNome nome ((var@(nome',_,_)):vars) = if nome == nome' then (Just var,0) else (var',n+1) where (var',n) = getVarFromNome nome vars

funcaoFold'' :: Estado -> Variavel -> Estado
funcaoFold'' estadoInicial variavel = 
    case addVariavel variavel estadoInicial of
        Right estado -> estado
        Left erro -> error $ "Deu erro"

evaluateExprs :: Estado -> [EXPR] -> ([Valor],[Tipo],Estado)
evaluateExprs estadoInicial [] = ([], [], estadoInicial)
evaluateExprs estadoInicial (expr:exprs) = (valor:valores,tipo:tipos,estadoFinal)
    where (valor, tipo, estadoIntermediario) = evaluateExpr estadoInicial expr
          (valores, tipos, estadoFinal) =  evaluateExprs estadoIntermediario exprs


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
getposTokenOp (OU p) = p
getposTokenOp (SlowOU p) = p
getposTokenOp (E p) = p
getposTokenOp (SlowE p) = p

evaluateExpr :: Estado -> EXPR -> (Valor,Tipo,Estado)

evaluateExpr estado (CRIAOU expr1 op expr2) = do
    case res1 of
        ValorLogico True -> (ValorLogico True, TipoAtomico "LOGICO", estado1)
        ValorLogico False ->
            case res2 of
                ValorLogico a -> (ValorLogico a, TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando OU:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando OU:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIASLOWOU expr1 op expr2) = do
    case res1 of
        ValorLogico a ->
            case res2 of
                ValorLogico b -> (ValorLogico (a || b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando ~OU:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando ~OU:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAE expr1 op expr2) = do
    case res1 of
        ValorLogico False -> (ValorLogico False, TipoAtomico "LOGICO", estado1)
        ValorLogico True ->
            case res2 of
                ValorLogico a -> (ValorLogico a, TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando E:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando E:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIASLOWE expr1 op expr2) = do
    case res1 of
        ValorLogico a ->
            case res2 of
                ValorLogico b -> (ValorLogico (a && b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando ~E:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando ~E:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2


evaluateExpr estado (CRIALESS expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a < b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador <:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a < b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando <:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a < b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador <:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a < b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador <:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador <:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIALEQ expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a <= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador <=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a <= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador <=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a <= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador <=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a <= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador <=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador <=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAEQUAL expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a == b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador =:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a == b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador =:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a == b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador =:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a == b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador =:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador =:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAGEQ expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a >= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador >=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a >= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador >=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a >= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador >=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a >= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador >=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador >=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAGREAT expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a > b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador >:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a > b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador >:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a > b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador >:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a > b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador >:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador >:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIADIFF expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a /= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador /=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a /= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador /=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a /= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador /=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a /= b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador /=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador /=:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAADD expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (a + b), TipoAtomico "INTEIRO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador +:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a + b), TipoAtomico "REAL", estado2)
                otherwise -> error $ "Tipos inválidos para o operador +:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorTexto (a ++ b), TipoAtomico "TEXTO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador +:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador +:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIASUB expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (a - b), TipoAtomico "INTEIRO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador -:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a - b), TipoAtomico "REAL", estado2)
                otherwise -> error $ "Tipos inválidos para o operador -:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador -:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2 

evaluateExpr estado (CRIAMULT expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (a * b), TipoAtomico "INTEIRO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador *:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a * b), TipoAtomico "REAL", estado2)
                otherwise -> error $ "Tipos inválidos para o operador *:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador *:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2 

evaluateExpr estado (CRIADIV expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (quot a b), TipoAtomico "INTEIRO", estado2)
                otherwise -> error $ "Tipos inválidos para o operador /:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a / b), TipoAtomico "REAL", estado2)
                otherwise -> error $ "Tipos inválidos para o operador /:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o operador /:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2 
        
evaluateExpr estado (CRIAMOD expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (mod a b), TipoAtomico "INTEIRO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando MOD:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando MOD:\nTipo esquerdo: " ++ (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2 

evaluateExpr estado (CRIANEG op expr) = do
    case res1 of
        ValorInteiro a -> (ValorInteiro (-a), TipoAtomico "INTEIRO", estado1)
        ValorReal a -> (ValorReal (-a), TipoAtomico "REAL", estado1)
        otherwise -> error $ "Tipo inválido para o operador unário -:\nTipo recebido: " ++ (show tipo1) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr

evaluateExpr estado (CRIANOT op expr) = do
    case res1 of
        ValorLogico a -> (ValorLogico (not a), TipoAtomico "LOGICO", estado1)
        otherwise -> error $ "Tipo inválido para o comando NOT:\nTipo recebido: " ++ (show tipo1) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr

evaluateExpr estado (CRIACONVERSAO tipo expr) = do
    case tipo of
        TIPO p "INTEIRO" -> 
            case res1 of
                ValorInteiro a -> (ValorInteiro a, TipoAtomico "INTEIRO", estado1)
                ValorReal a -> (ValorInteiro (read (show a)), TipoAtomico "INTEIRO", estado1)
                ValorCaractere a -> (ValorInteiro (read (show a)), TipoAtomico "INTEIRO", estado1)
                ValorTexto a -> (ValorInteiro (read a), TipoAtomico "INTEIRO", estado1)
                ValorLogico a -> (ValorInteiro (logicoToInt a), TipoAtomico "INTEIRO", estado1)
                otherwise -> error $ "Conversão inválida para INTEIRO\nTipo recebido: " ++ (show tipo1) ++ "\nPosição: " ++ show p
        TIPO p "REAL" -> 
            case res1 of
                ValorInteiro a -> (ValorReal (read (show a)), TipoAtomico "REAL", estado1)
                ValorReal a -> (ValorReal a, TipoAtomico "REAL", estado1)
                ValorCaractere a -> (ValorReal (read (show a)), TipoAtomico "REAL", estado1)
                ValorTexto a -> (ValorReal (read a), TipoAtomico "REAL", estado1)
                ValorLogico a -> (ValorReal (logicoToReal a), TipoAtomico "REAL", estado1)
                otherwise -> error $ "Conversão inválida para REAL\nTipo recebido: " ++ (show tipo1) ++ "\nPosição: " ++ show p
        TIPO p "TEXTO" ->
            case res1 of
                ValorInteiro a -> (ValorTexto (show a), TipoAtomico "TEXTO", estado1)
                ValorReal a -> (ValorTexto (show a), TipoAtomico "TEXTO", estado1)
                ValorCaractere a -> (ValorTexto (show a), TipoAtomico "TEXTO", estado1)
                ValorTexto a -> (ValorTexto a, TipoAtomico "TEXTO", estado1)
                ValorLogico a -> (ValorTexto (showLogico a), TipoAtomico "TEXTO", estado1)
                otherwise -> error $ "Conversão inválida para TEXTO\nTipo recebido: " ++ (show tipo1) ++ "\nPosição: " ++ show p
        TIPO p "CARACTERE" ->
            case res1 of
                ValorCaractere a -> (ValorCaractere a, TipoAtomico "CARACTERE", estado1)
                otherwise -> error $ "Conversão inválida para CARACTERE\nTipo recebido: " ++ (show tipo1) ++ "\nPosição: " ++ show p
        TIPO p "LOGICO" ->
            case res1 of
                ValorInteiro a -> (ValorLogico (intToLogico a), TipoAtomico "LOGICO", estado1)
                ValorReal a -> (ValorLogico (realToLogico a), TipoAtomico "LOGICO", estado1)
                ValorLogico a -> (ValorLogico a, TipoAtomico "LOGICO", estado1)
                otherwise -> error $ "Conversão inválida para LOGICO\nTipo recebido: " ++ (show tipo1) ++ "\nPosição: " ++ show p
        TIPO p s -> error $ "Conversão inexistente para " ++ s ++ "\nTipo recebido: " ++ (show tipo1) ++ "\nPosição: " ++ show p
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr
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
        otherwise -> error $ "Busca por valor em variável que não é um ponteiro:\nTipo: " ++ (show tipo) ++ "\nPosição: " ++ (show p)
    where 
        (val, tipo, estado1) = evaluateExpr estado (CRIAVAR var)
        getValor :: (Either ErroEstado Variavel) -> Valor
        getValor (Left _) = error $ "Ponteiro aponta para posição inválida:\nTipo: " ++ (show tipo) ++ "\nPosição: " ++ (show p)
        getValor (Right (_,_,val)) = val
        getTipoApontado :: Tipo -> Tipo
        getTipoApontado (TipoPonteiroFim s) = TipoAtomico s
        getTipoApontado (TipoPonteiroRecursivo s) = s
        getTipoApontado _ = error $ "Erro impossível de ocorrer"

evaluateExpr estado (CRIAVALOREXPR (CRIASEQVAL (VALOR p) seqval)) = 
    case val of
        ValorPonteiro s -> (getValor $ getVariavel s estado1, getTipoApontado tipo, estado1)
        otherwise -> error $ "Busca por valor em variável que não é um ponteiro:\nTipo: " ++ (show tipo) ++ "\nPosição: " ++ (show p)
    where 
        (val, tipo, estado1) = evaluateExpr estado (CRIAVALOREXPR seqval)
        getValor :: (Either ErroEstado Variavel) -> Valor
        getValor (Left _) = error $ "Ponteiro aponta para posição inválida:\nTipo: " ++ (show tipo) ++ "\nPosição: " ++ (show p)
        getValor (Right (_,_,val)) = val
        getTipoApontado :: Tipo -> Tipo
        getTipoApontado (TipoPonteiroFim s) = TipoAtomico s
        getTipoApontado (TipoPonteiroRecursivo s) = s
        getTipoApontado _ = error $ "Erro impossível de ocorrer"

-- variável simples
evaluateExpr estado (CRIAVAR (Var [SingleVar (ID posicao nome) (OptionalSQBrack [])])) = 
    case (getVariavel nome estado) of
        Right (_,tipo,valor) -> (valor,tipo,estado)
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)

-- variáveis simples com acesso a vetor
evaluateExpr estado (CRIAVAR (Var [SingleVar (ID posicao nome) (OptionalSQBrack ids)])) =
    case (getVariavel nome estado) of
        Right (_, TipoVetor faixas etc, valor) ->
            case (evaluateVet estado valor (TipoVetor faixas etc) faixas ids) of
                Right result -> result
                Left err     -> error $ err ++ "\nPosição: " ++ (show posicao)
        Right a -> error $ "Variável " ++ nome ++ " não é um vetor\nPosição: " ++ (show posicao)
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)

-- variáveis com acesso a campo de estrutura
evaluateExpr estado (CRIAVAR (Var ((SingleVar (ID posicao nome) (OptionalSQBrack [])):snglVars))) =
    case (getVariavel nome estado) of
        Right (_, TipoEstrutura nome_estr _, valor_estr) ->
            case evaluateEstr estado valor_estr snglVars of
                Right result -> result
                Left erro -> error $ nome_estr ++ " " ++ (show erro) ++ "\nPosição: " ++ (show posicao)
        Right _ -> error $ "Variável " ++ nome ++ " não é uma estrutura\nPosição: " ++ (show posicao)
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)

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
                        Left err -> error $ err ++ "\nPosição: " ++ (show posicao)
                Right _  -> error $ nome ++ " não é um vetor de estruturas\nPosição: " ++ (show posicao)
                Left err -> error $ err ++ "\nPosição: " ++ (show posicao)
        Right _ -> error $ "Variável " ++ nome ++ " não é um vetor\nPosição: " ++ (show posicao)
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)

-- Alocação dinâmica de um elemento
evaluateExpr prevEstado@(escopo, tipos, subprogs, prevCont) (CRIANOVO ponts tipo (OptionalSQBrack [])) =
    case res of
        Right estadoAtualizado -> (ValorPonteiro nome', criaPonteiro tipo', estadoAtualizado)
        Left erro -> error ("erro impossível de acontecer: " ++ show erro)
    where
        id = "$" ++ show prevCont 
        cont = prevCont+1
        estado = (escopo, tipos, subprogs, cont)
        ((nome', tipo'), estadoIntermediario) =
            getDec ponts tipo id estado
        res = addVariavelGlobal (nome', tipo', getValorInicial tipo') estadoIntermediario
        criaPonteiro :: Tipo -> Tipo
        criaPonteiro t@(TipoPonteiroFim _) = TipoPonteiroRecursivo t
        criaPonteiro t@(TipoPonteiroRecursivo _) = TipoPonteiroRecursivo t
        criaPonteiro t@(TipoAtomico s) = TipoPonteiroFim s
        criaPonteiro t@(TipoEstrutura s _) = TipoPonteiroFim s
        getDec :: [PONT] -> Token -> String -> Estado -> (Declaracao, Estado)
        getDec ponteiros (TIPO posicao nome) variavel estado =
            case tipoPrimitivo of
                Right tipoEncontrado -> ((variavel, getTipoPonteiro ponteiros tipoEncontrado), estado)
                Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
            where tipoPrimitivo = getTipo nome estado

evaluateExpr estado (CRIACHAMADAFUNC (CRIACHAMADA (ID posicao nome) exprs)) =
    case subprograma of
        Right (Left (nome, declaracoes, stmts)) ->
            error $ "Procedimento usado em expressão\nPosição: " ++ show posicao
        Right (Right (nome, declaracoes, stmts, tipoRetorno)) -> 
            let estadoFinal = foldl funcaoFold'' estadoAtualizado (zip3 (fst $ unzip declaracoes) tiposParametros valoresParametros) in
            unsafePerformIO $ (rodaStmts stmts estadoFinal) >>= (\(estado1, a, b, c, d, e) -> if isJust d then ((return (evaluateExpr estado1 (fromJust d))) >>= (\(valor, tipo, estado2) -> return (valor, tipo, removerEscopo estado2))) else error $ "erro")
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where estadoEscopoFuncao = criarEscopo (getIdEscopoAtual estado) estado
          (valoresParametros, tiposParametros, estadoAtualizado) = evaluateExprs estadoEscopoFuncao exprs
          subprograma = getSubprograma nome tiposParametros estadoAtualizado

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
                Left err -> error $ err ++ "\nPosição: " ++ (show posicao)
        Just _ -> error $ "Variável " ++ nome ++ " não é um vetor\nPosição: " ++ (show posicao)
        Nothing -> Left $ "não possui o campo " ++ nome

-- Avalia uma estrutura para vários acessos à campo. Ex.: a.first.b.k.p
evaluateEstr estado (ValorEstrutura vars_estr) ((SingleVar (ID posicao nome) (OptionalSQBrack [])):snglVars) = 
    -- procura o primeiro campo 'nome' na lista de campos da estrutura
    case (getVariavelTabela nome vars_estr) of
        -- Se for outra esrtrutura, procura para o resto dos campos
        Just (_, TipoEstrutura _ _, valor_estr) ->  evaluateEstr estado valor_estr snglVars
        Just _ -> error $ nome ++ " não é uma estrutura\nPosição: " ++ (show posicao)
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
                Right _ -> error $ nome ++ " não é uma estrutura\nPosição: " ++ (show posicao)
                Left err -> error $ err ++ "\nPosição: " ++ (show posicao)
        Just _ -> error $ nome ++ " não é uma vetor\nPosição: " ++ (show posicao)
        Nothing -> Left $ "não possui o campo " ++ nome
{-
    estado
    vetor
    limites
    indices

    retorna (Valor,Estado)
-}
evaluateVet :: Estado -> Valor  -> Tipo -> [Integer] -> [EXPR] -> Either String (Valor,Tipo,Estado)
evaluateVet _ _ _ [] (_:_) = Left "O número de índices é maior que o número de dimensões no vetor"
evaluateVet _ _ _ (_:_) [] = Left "O número de índices é menor que o número de dimensões no vetor"
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
        (ValorEstrutura _, _, _) -> Left $ "ESTRUTURA (" ++ (show tipo) ++ ") como subscrito de vetor"
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
        (ValorEstrutura _, tipo, _) -> Left $ "ESTRUTURA (" ++ (show tipo) ++ ") passada como subscrito de vetor"
    where
        res_expr = evaluateExpr estado expr

getIth :: [t] -> Integer -> Either String t
getIth [] _ = Left "Indice fora de faixa"
getIth (a:b) i
    | i < 1     = Left "Indice fora de faixa" 
    | i == 1    = Right a
    | otherwise = getIth b (i-1)

traduzTipo :: Tipo -> Estado -> Tipo
traduzTipo (TipoAtomico s) estado = 
    case (getTipo s estado) of
        Right p -> p
        Left erro -> error $ show erro
traduzTipo p estado = p

--Leitura de uma palavra nao vazia
getPalavra :: IO String
getPalavra = do
    s <- getPalavra'
    if null s then
        getPalavra
    else
        return s

--Leitura de uma palavra possivelmente vazia
getPalavra' :: IO String
getPalavra' = handle handleEOF $ do
    c <- getChar
    if isSpace c then return [] else (c:) <$> getPalavra'
    where handleEOF e = if isEOFError e then return [] else throwIO e

