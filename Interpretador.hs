module Interpretador where

import Data.Fixed
import Data.List
import Data.Maybe
import Text.Read
import System.IO
import Tipos
import Estado
import Lexico
import Arvore
import Expressoes
import Debug.Trace

--Estado antes da execucao
estadoinicial = ([], TipoAtomico "INTEIRO":TipoAtomico "REAL":TipoAtomico "LOGICO":TipoAtomico "TEXTO":TipoAtomico "CARACTERE":[], [])

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
funcaoFold' tipo (tipos, estadoInicial) token = ((tipo':tipos), estadoFinal)
    where (tipo', estadoFinal) = (getTipoVetor tipo token estadoInicial)

getDecs :: [DEC] -> Estado -> ([Declaracao], Estado)
getDecs [] estado = ([], estado)
getDecs ((NOVADEC ponteiros (TIPO posicao nome) tokensVariaveis):declaracoes) estado =
    case tipoPrimitivo of
        Right tipoEncontrado -> 
            let (tipos, estadoIntermediario) = f tipoEncontrado
                (declaracoes', estadoFinal) = getDecs declaracoes estadoIntermediario in
            trace (show estadoFinal) ((zip variaveis (map (getTipoPonteiro ponteiros) tipos)) ++ declaracoes', estadoFinal)
        Left erro -> error $ (show erro) ++ ": posição " ++ (show posicao)
    where tipoPrimitivo = getTipo nome estado
          f tipo = foldl (funcaoFold' tipo) ([], estado) tokensVariaveis
          variaveis = map getNomeVar tokensVariaveis

getTipoPonteiro :: [PONT] -> Tipo -> Tipo
getTipoPonteiro [] tipo = tipo
getTipoPonteiro [pont] (TipoAtomico nome) = TipoPonteiroFim nome
getTipoPonteiro [pont] (TipoEstrutura nome _) = TipoPonteiroFim nome
getTipoPonteiro (pont:ponts) tipo = TipoPonteiroRecursivo $ getTipoPonteiro ponts tipo

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
funcaoFold (valores, estadoInicial) expr = (((getValorInteiro valor):valores), estadoFinal)
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
    where ((nome', tipo'):_, estadoIntermediario) = getDecs [declaracao] estado
          res = addVariavel (nome', tipo', getValorInicial tipo') estadoIntermediario
          posicao = getPosicaoSingleVar id

addDec declaracao@(NOVADEC pont tipo ((VAR_COM (CRIAATRIB id expr)):b)) estado =
    case res of
        Right estadoAtualizado -> addDec (NOVADEC pont tipo b) estadoAtualizado
        Left erro -> fail $ (show erro) ++ ": posição " ++ (show posicao)
    where ((nome', tipo'):_, estadoIntermediario) = getDecs [declaracao] estado
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
        otherwise -> error $ "Tipo da expressão não LOGICO no ENQUANTO: posição: " ++ show p

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
executarStmt (NOVOATRIBSTMT expr expr') estado =
    case estadoAtualizado  of
        Right estado' -> return (estado', False, False, False, Nothing, Nothing)
        Left erro -> fail $ show erro
    where (nome, tipo, _) = getVariavelFromExpr expr estado
          (valor, _, estadoIntermediario) = evaluateExpr estado expr'
          estadoAtualizado = atualizarVariavel (nome, tipo, valor) estadoIntermediario

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
                    Nothing -> error $ "Tipo da variável '" ++ nome ++ "' não é INTEGER: posição: " ++ (show p)
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
                    Nothing -> error $ "Tipo da variável '" ++ nome ++ "' não é INTEGER: posição: " ++ (show p)
            else
                let valorEstrutura = incrementaValorEstrutura campos valor in
                case atualizarVariavel (nome, tipo, valorEstrutura) estado of
                    Right estado' -> return (estado', False, False, False, Nothing, Nothing)
                    Left erro -> fail $ show erro ++ ": posição " ++ (show p)
        Left erro -> fail $ show erro ++ ": posição " ++ (show p)

executarStmt (NOVOCHAMADA cHAMADA) estado = undefined

executarStmt (NOVOSE (CRIASE token expr stmts1 (OptionalSenao stmts2))) estado =
    case res of
        ValorLogico True -> iniciaBlocoSe stmts1 estado1
        ValorLogico False -> iniciaBlocoSe stmts2 estado1
        otherwise -> fail $ "Expressao não retorna LOGICO: posição: " ++ (show getPosSE)
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
executarStmt (NOVOLEIA (CRIALEIA (LEIA p) (expr:exprs))) estado = do
    let (_, tipo, valor) = getVariavelFromExpr (CRIAVAR expr) estado
    let tipoAtualizado = case tipo of { TipoAtomico s -> tipo; TipoVetor s tipoPrimitivo -> tipoPrimitivo }
    case tipoAtualizado of
        TipoAtomico "INTEIRO" -> do
            hFlush stdout
            s <- getLine
            case readMaybe s :: Maybe Integer of
                Just i -> executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (CRIAINT (INTEIRO p i))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
                Nothing -> error $ "Valor não permitido como INTEIRO: posição: " ++ (show p)
        TipoAtomico "REAL" -> do
            hFlush stdout
            s <- getLine
            case readMaybe s :: Maybe Double of
                Just i -> executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (CRIAREAL (REAL p i))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
                Nothing -> error $ "Valor não permitido como REAL: posição: " ++ (show p)
        TipoAtomico "CARACTERE" -> do
            hFlush stdout
            s <- getLine
            case readMaybe s :: Maybe Char of
                Just i -> executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (CRIACARACTERE (CARACTERE p i))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
                Nothing -> error $ "Valor não permitido como CARACTERE: posição: " ++ (show p)
        TipoAtomico "TEXTO" -> do
            hFlush stdout
            s <- getLine
            case readMaybe s :: Maybe String of
                Just i -> executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (CRIATEXTO (TEXTO p i))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
                Nothing -> error $ "Valor não permitido como TEXTO: posição: " ++ (show p)
        TipoAtomico "LOGICO" -> do
            hFlush stdout
            s <- getLine
            case readMaybe s :: Maybe Bool of
                Just i -> executarStmt (NOVOATRIBSTMT (CRIAVAR expr) (CRIALOGICO (LOGICO p i))) estado >>= (\(estado1,_,_,_,_,_) -> executarStmt (NOVOLEIA (CRIALEIA (LEIA p) exprs)) estado1)
                Nothing -> error $ "Valor não permitido como LOGICO: posição: " ++ (show p)
        otherwise -> error $ "Comando LEIA para tipo não primitivo: posição: " ++ show p

executarStmt (NOVOBLOCO (CRIABLOCO stmts)) estado =
    iniciaBloco stmts estado

getVariavelFromExpr :: EXPR -> Estado -> Variavel
getVariavelFromExpr (CRIAVAR (Var ((SingleVar (ID posicao nome) colchetes):vars))) estado = 
    case var of
        Right var' -> var'
        Left erro -> error $ (show erro) ++ ": posição: " ++ (show posicao)
    where var = getVariavel nome estado

getVariavelFromExpr _ _ = error $ "deu erro"


incrementaValorEstrutura :: [SingleVAR] -> Valor -> Valor
incrementaValorEstrutura ((SingleVar (ID p nomeCampo) _):_) (ValorEstrutura []) = error $ "Campo '" ++ nomeCampo ++ "'' não encontrado posição: " ++ (show p)
incrementaValorEstrutura nomes@((SingleVar (ID p nomeCampo) _):nomeCampos) (ValorEstrutura (campo@(nome, tipo, valor):campos)) =
    if nomeCampo == nome then
        if null nomeCampos then
            case getValorInteiro valor of
                Just valor' -> ValorEstrutura ((nome, tipo, ValorInteiro (succ valor')):campos)
                Nothing -> error $ "Tipo da variável '" ++ nome ++ "' não é INTEGER: posição: " ++ (show p)
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
                Nothing -> error $ "Tipo da variável '" ++ nome ++ "' não é INTEGER: posição: " ++ (show p)
        else
            ValorEstrutura ((nome, tipo, (incrementaValorEstrutura nomeCampos valor)):campos)
    else
        ValorEstrutura (campo:camposAtualizado)
    where (ValorEstrutura camposAtualizado) = incrementaValorEstrutura nomes (ValorEstrutura campos)
    
