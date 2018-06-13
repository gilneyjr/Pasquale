module Interpretador where

import Data.Fixed
import Data.List
import Data.Maybe
import Text.Read
import System.IO
import Debug.Trace
import Tipos
import Estado
import Lexico
import Arvore
import Expressoes

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
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
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
                error $ (show erro) ++ ": posição " ++ (show posicao)
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
        Left erro -> error $ (show erro) ++ ": posição " ++ (show posicao)
    where tipoPrimitivo = getTipo nome estado
          variaveis = map getNomeVar tokensVariaveis

getTipoVetor :: Tipo -> VAR_ -> Estado -> (Tipo, Estado)
getTipoVetor tipo (VAR_SEM (SingleVar _ (OptionalSQBrack []))) estado = (tipo, estado)
getTipoVetor tipo (VAR_COM (CRIAATRIB (SingleVar _ (OptionalSQBrack [])) _)) estado = (tipo, estado)
getTipoVetor tipo (VAR_SEM (SingleVar posicao (OptionalSQBrack exprs))) estado =
    if all isJust valores then
        (TipoVetor (catMaybes valores) tipo, estadoFinal)
    else
        error $ "Expressão não é um valor inteiro valido: posição: " ++ (show posicao)
    where (valores, estadoFinal) = foldl funcaoFold ([], estado) exprs

getTipoVetor tipo (VAR_COM (CRIAATRIB (SingleVar posicao (OptionalSQBrack exprs)) _)) estado =
    if all (\v -> isJust v) valores then
        (TipoVetor (catMaybes valores) tipo, estadoFinal)
    else
        error $ "Expressão não é um valor inteiro valido: posição: " ++ (show posicao)
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
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where ((nome', tipo'):_, estadoIntermediario) = getDecs [(NOVADEC pont tipo [(VAR_SEM id)])] estado
          res = addVariavel (nome', tipo', getValorInicial tipo') estadoIntermediario
          posicao = getPosicaoSingleVar id

addDec declaracao@(NOVADEC pont tipo ((VAR_COM (CRIAATRIB id expr)):b)) estado =
    case res of
        Right estadoAtualizado -> addDec (NOVADEC pont tipo b) estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
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
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where (subprograma, estadoIntermediario) = getSubprogFromFunc func estado
          novo = addSubprograma subprograma estadoIntermediario
          posicao = getPosicaoFunc func
addSubprogs ((CRIAPROC proc):b) estado =
    case novo of
        Right estadoAtualizado -> addSubprogs b estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where (subprograma, estadoIntermediario) = getSubprogFromProc proc estado
          novo = addSubprograma subprograma estadoIntermediario
          posicao = getPosicaoProc proc
addSubprogs ((CRIAOPER oper):b) estado =
    case novo of
        Right estadoAtualizado -> addSubprogs b estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
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
        Left erro -> error $ (show erro) ++ ": posição " ++ (show posicao)
    where tipoPrimitivo = getTipo nome estado

--Insere o escopo da Main, executa, e depois remove
iniciaBlocoMain :: MAIN -> Estado -> IO EstadoCompleto
iniciaBlocoMain (Main stmts) estado = do
    (estado1, temRetorno, temSaia, temContinue, maybeExpr, maybePos) <- rodaStmts stmts (criarEscopo 1 estado)
    case (temRetorno, temSaia, temContinue) of
        (_, False, False) -> 
            case maybeExpr of
                Nothing -> return (removerEscopo estado1, temRetorno, temSaia, temContinue, maybeExpr, maybePos)
                otherwise -> error $ "Erro retorno não vazio: posição: " ++ (show $ fromJust maybePos) 
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
        otherwise -> error $ "Tipo da expressão não LOGICO na condição do ENQUANTO: posição: " ++ show p

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
        TipoEstrutura _ _ ->
            let (estruturaNova, estadoFinal) = (atualizarEstrutura expr valorAntigo valor tipoExpr posicao estadoIntermediario) in
            case atualizarVariavel (nome, tipo, estruturaNova) estadoFinal  of
                Right estado' -> return (estado', False, False, False, Nothing, Nothing)
                Left erro -> fail $ show erro
        TipoVetor dimensoes _ ->
            let (vetorNovo, estadoFinal) = (atualizarVetor expr tipo dimensoes valorAntigo valor tipoExpr posicao estadoIntermediario) in
            case atualizarVariavel (nome, tipo, vetorNovo) estadoFinal  of
                Right estado' -> return (estado', False, False, False, Nothing, Nothing)
                Left erro -> fail $ show erro
        otherwise ->
            case atualizarVariavel (nome, tipo, valor) estadoIntermediario  of
                Right estado' -> return (estado', False, False, False, Nothing, Nothing)
                Left erro -> fail $ show erro
    where ((nome, tipo, valorAntigo), estado) = getVariavelFromExpr expr estado0
          (valor, tipoExpr, estadoIntermediario) = evaluateExpr estado expr'

executarStmt (NOVOINC (CRIAINC (Var (tokenNome@(SingleVar (ID p nomeCampo) _):campos)))) estado =
    case getVariavel nomeCampo estado of
        Right (nome, tipo, valor) ->
            if null campos then
                case getValorInteiro valor of
                    Just valor' ->
                        case atualizarVariavel (nome, tipo, ValorInteiro (succ valor')) estado of
                            Right estado' -> do
                                return (estado', False, False, False, Nothing, Nothing)
                            Left erro -> fail $ show erro ++ ": posição " ++ (show p)
                    Nothing -> error $ "Tipo da variável '" ++ nome ++ "' não é INTEIRO: posição: " ++ (show p)
            else
                let valorEstrutura = incrementaValorEstrutura campos valor in
                case atualizarVariavel (nome, tipo, valorEstrutura) estado of
                    Right estado' -> return (estado', False, False, False, Nothing, Nothing)
                    Left erro -> fail $ show erro ++ ": posição " ++ (show p)
        Left erro -> fail $ show erro ++ ": posição " ++ (show p)

executarStmt (NOVODECR (CRIADECR (Var (tokenNome@(SingleVar (ID p nomeCampo) _):campos)))) estado = 
    case getVariavel nomeCampo estado of
        Right (nome, tipo, valor) ->
            if null campos then
                case getValorInteiro valor of
                    Just valor' ->
                        case atualizarVariavel (nome, tipo, ValorInteiro (valor' - 1)) estado of
                            Right estado' -> return (estado', False, False, False, Nothing, Nothing)
                            Left erro -> fail $ show erro ++ ": posição " ++ (show p)
                    Nothing -> error $ "Tipo da variável '" ++ nome ++ "' não é INTEIRO: posição: " ++ (show p)
            else
                let valorEstrutura = incrementaValorEstrutura campos valor in
                case atualizarVariavel (nome, tipo, valorEstrutura) estado of
                    Right estado' -> return (estado', False, False, False, Nothing, Nothing)
                    Left erro -> fail $ show erro ++ ": posição " ++ (show p)
        Left erro -> fail $ show erro ++ ": posição " ++ (show p)

executarStmt (NOVOCHAMADA (CRIACHAMADA (ID posicao nome) exprs)) estado =
    case subprograma of
        Right (Left (nome, declaracoes, stmts)) ->
            let estadoFinal = foldl funcaoFold'' estadoAtualizado (zip3 (fst $ unzip declaracoes) tiposParametros valoresParametros) in 
            (rodaStmts stmts estadoFinal) >>= (\(estado1, a, b, c, d, e) -> return (removerEscopo estado1, a, b, c, d, e))
        Right (Right (nome, declaracoes, stmts, tipoRetorno)) -> 
            let estadoFinal = foldl funcaoFold'' estadoAtualizado (zip3 (fst $ unzip declaracoes) tiposParametros valoresParametros) in
            (rodaStmts stmts estadoFinal) >>= (\(estado1, a, b, c, d, e) -> return (removerEscopo estado1, a, b, c, d, e))
        Left erro -> error $ (show erro) ++ ": posição: " ++ (show posicao)
    where estadoEscopoFuncao = criarEscopo (getIdEscopoAtual estado) estado
          (valoresParametros, tiposParametros, estadoAtualizado) = evaluateExprs estadoEscopoFuncao exprs
          subprograma = getSubprograma nome tiposParametros estadoAtualizado

executarStmt (NOVOSE (CRIASE token expr stmts1 (OptionalSenao stmts2))) estado =
    case res of
        ValorLogico True -> iniciaBlocoSe stmts1 estado1
        ValorLogico False -> iniciaBlocoSe stmts2 estado1
        otherwise -> fail $ "Expressão não é do tipo LOGICO: posição: " ++ (show getPosSE)
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
        otherwise -> error $ "Operação de delete para expressão que não retorna Ponteiro: posição: " ++ (show $ getPosFromDelete tok)
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
        otherwise -> error $ "Comando ESCREVA para tipo não primitivo: posição: " ++ show p
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
            s <- getLine
            case readMaybe s :: Maybe Integer of
                Just i -> executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (Attrib p) (CRIAINT (INTEIRO p i))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
                Nothing -> error $ "Valor não permitido como INTEIRO: posição: " ++ (show p)
        TipoAtomico "REAL" -> do
            hFlush stdout
            s <- getLine
            case readMaybe s :: Maybe Double of
                Just i -> executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (Attrib p) (CRIAREAL (REAL p i))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
                Nothing -> error $ "Valor não permitido como REAL: posição: " ++ (show p)
        TipoAtomico "CARACTERE" -> do
            hFlush stdout
            s <- getLine
            case readMaybe s :: Maybe Char of
                Just i -> executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (Attrib p) (CRIACARACTERE (CARACTERE p i))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
                Nothing -> error $ "Valor não permitido como CARACTERE: posição: " ++ (show p)
        TipoAtomico "TEXTO" -> do
            hFlush stdout
            s <- getLine
            case readMaybe s :: Maybe String of
                Just i -> executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (Attrib p) (CRIATEXTO (TEXTO p i))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
                Nothing -> error $ "Valor não permitido como TEXTO: posição: " ++ (show p)
        TipoAtomico "LOGICO" -> do
            hFlush stdout
            s <- getLine
            case readMaybe s :: Maybe Bool of
                Just i -> executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (Attrib p) (CRIALOGICO (LOGICO p i))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
                Nothing -> error $ "Valor não permitido como LOGICO: posição: " ++ (show p)
        otherwise -> error $ "Comando LEIA para tipo não primitivo: posição: " ++ show p

executarStmt (NOVOBLOCO (CRIABLOCO stmts)) estado =
    iniciaBloco stmts estado

getVariavelFromExpr :: EXPR -> Estado -> (Variavel, Estado)
getVariavelFromExpr (CRIAVAR (Var ((SingleVar (ID posicao nome) colchetes):_))) estado = 
    case var of
        Right var' -> (var', estado)
        Left erro -> error $ (show erro) ++ ": posição: " ++ (show posicao)
    where var = getVariavel nome estado

getVariavelFromExpr (CRIAVALOREXPR (CRIAULTVAL (VALOR p) var)) estadoAntigo =
    case res of
        ValorPonteiro nome -> case var of
            Right var' -> (var', estado)
            Left erro -> error $ "Ponteiro acessado aponta para posicao invalida: posição: " ++ (show p)
            where var = getVariavel nome estado
        otherwise -> error $ "Busca por valor em variável que não é um ponteiro: posição: " ++ (show p) ++ ", tipo: " ++ (show tipo)
    where
        (res, tipo, estado) = evaluateExpr estadoAntigo (CRIAVAR var)

getVariavelFromExpr (CRIAVALOREXPR (CRIASEQVAL (VALOR p) val)) estadoAntigo =
    case res of
        ValorPonteiro nome -> case var of
            Right var' -> (var', estado)
            Left erro -> error $ "Ponteiro acessado aponta para posicao invalida: posição: " ++ (show p)
            where var = getVariavel nome estado
        otherwise -> error $ "Busca por valor em variável que não é um ponteiro: posição: " ++ (show p) ++ ", tipo: " ++ (show tipo)
    where
        (res, tipo, estado) = evaluateExpr estadoAntigo (CRIAVALOREXPR val)

incrementaValorEstrutura :: [SingleVAR] -> Valor -> Valor
incrementaValorEstrutura ((SingleVar (ID p nomeCampo) _):_) (ValorEstrutura []) = error $ "Campo '" ++ nomeCampo ++ "'' não encontrado: posição: " ++ (show p)
incrementaValorEstrutura nomes@((SingleVar (ID p nomeCampo) _):nomeCampos) (ValorEstrutura (campo@(nome, tipo, valor):campos)) =
    if nomeCampo == nome then
        if null nomeCampos then
            case getValorInteiro valor of
                Just valor' -> ValorEstrutura ((nome, tipo, ValorInteiro (succ valor')):campos)
                Nothing -> error $ "Tipo da variável '" ++ nome ++ "' não é do tipo INTEIRO: posição: " ++ (show p)
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
                Nothing -> error $ "Tipo da variável '" ++ nome ++ "' não é do tipo INTEIRO: posição: " ++ (show p)
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
    error $ "Muitos subscritos no acesso ao arranjo: posição: " ++ (show pos)

atualizarVetor' valoresVetor (TipoVetor _ tipoElem) (dimensao:dimensoes) [posicao] valorNovo tipoNovo pos =
    if (posicao >= 1) && (posicao <= dimensao) then
        if dimensoes == [] then
            if tipoElem == tipoNovo then
                   inicio ++ [valorNovo] ++ (tail fim)
            else
                error $ "Atribuição inválida!\nTipo esperado: " ++ (show tipoElem) ++ "\nTipo recebido: " ++ (show tipoNovo) ++ "\nposição: " ++ (show pos)
        else
            error $ "Poucos subscritos no acesso ao arranjo: posição: " ++ (show pos)
    else
        error $ "Segmentation fault!\nposição: " ++ show pos
    where (inicio, fim) = genericSplitAt (posicao - 1) valoresVetor

atualizarVetor' valoresVetor tipoVetor (dimensao:dimensoes) (posicao:posicoes) valorNovo tipoNovo pos = 
    if (posicao >= 1) && (posicao <= dimensao) then
        (inicio ++ [ValorVetor (atualizarVetor' valoresAntigos tipoVetor dimensoes posicoes valorNovo tipoNovo pos)] ++ fim)
    else
        error $ "Segmentation fault!\nposição: " ++ show pos
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
    error $ "Muitos subscritos no acesso à estrutura: posição: " ++ (show pos)

atualizarEstrutura' [variavel@(SingleVar (ID posicao nome) (OptionalSQBrack expr))] variaveisAntigas valorNovo tipoNovo pos estado =
    case var of
        Just (nomeVar, tipoVar, valor) -> 
            if null expr then
                if tipoVar == tipoNovo then
                    ((inicio ++ [(nomeVar, tipoVar, valorNovo)] ++ (tail fim)), estado)
                else error $ "Atribuição inválida!\nTipo esperado: " ++ (show tipoVar) ++ "\nTipo recebido: " ++ (show tipoNovo) ++ "\nposição: " ++ (show pos)
            else
                let (TipoVetor dimensoes _) = tipoVar
                    (valorVetor, estadoFinal) = (atualizarVetor (CRIAVAR (Var [variavel])) tipoVar dimensoes valor valorNovo tipoNovo pos estado) in
                ((inicio ++ [(nomeVar, tipoVar, valorVetor)] ++ (tail fim)), estadoFinal)
        Nothing -> error $ "Campo '" ++ nome ++ "' não definido na estrutura: posição: " ++ (show pos)
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
                    (valorVetor, estadoIntermediario) = atualizarVetor (CRIAVAR (Var [variavel])) tipoVar dimensoes valor valorNovo tipoNovo pos estado in
                ((inicio ++ [(nomeVar, tipoVar, valorVetor)] ++ (tail fim)), estadoIntermediario)
        Nothing -> error $ "Campo '" ++ nome ++ "' não definido na estrutura: posição: " ++ (show pos)
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