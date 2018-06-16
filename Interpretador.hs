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
inicializarPrograma = criarEscopo 0 estadoInicial

-- Adiciona as estruturas criadas pelo usuario
addEstrs :: [ESTR] -> Estado -> IO Estado
-- Caso não hajam estruturas para adicionar
addEstrs []    estado = return estado
-- Caso hajam estruturas para adicionar
addEstrs (a:b) estado =
    -- Caso tenha adicionado com sucesso, chama recursivamente para o resto das estruturas
    case novo of
        Right estadoAtualizado -> addEstrs b estadoAtualizado
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
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
            let (tipos, estadoIntermediario) = foldl (funcaoFold' (getTipoPonteiro ponteiros tipoEncontrado)) ([], estado) tokensVariaveis
                (declaracoes', estadoFinal) = getDecsEstr nomeEstrutura declaracoes estadoIntermediario in
                    (zip variaveis tipos ++ declaracoes', estadoFinal)
        Left erro -> 
            if nomeEstrutura == nome then
            case ponteiros of
                a:b ->
                    let tipoEncontrado = TipoPonteiroFim nomeEstrutura
                        (tipos, estadoIntermediario) = foldl (funcaoFold' (getTipoPonteiro b tipoEncontrado)) ([], estado) tokensVariaveis
                        (declaracoes', estadoFinal) = getDecsEstr nomeEstrutura declaracoes estadoIntermediario in
                            (zip variaveis tipos ++ declaracoes', estadoFinal)
                otherwise -> error $ "Estrutura " ++ nomeEstrutura ++ " contém ela mesma\nPosição: " ++ show posicao
            else
                error $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where tipoPrimitivo = getTipo nome estado
          variaveis = map getNomeVar tokensVariaveis

funcaoFold' :: Tipo -> ([Tipo], Estado) -> VAR_ -> ([Tipo], Estado)
funcaoFold' tipo (tipos, estado0) token = ((tipos ++ [tipo']), estadoFinal)
    where (tipo', estadoFinal) = (getTipoVetor tipo token estado0)

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
funcaoFold (valores, estado0) expr = ((valores ++ [getValorInteiro valor]), estadoFinal)
    where (valor, _, estadoFinal) = evaluateExpr estado0 expr

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
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where ((nome', tipo'):_, estadoIntermediario) = getDecs [(NOVADEC pont tipo [(VAR_SEM id)])] estado
          res = addVariavel (nome', tipo', getValorInicial tipo') estadoIntermediario
          posicao = getPosicaoSingleVar id

addDec declaracao@(NOVADEC pont tipo ((VAR_COM (CRIAATRIB id expr)):b)) estado =
    if mesmoTipo tipo' tipoExpr estado' then
        case res of
            Right estadoAtualizado -> addDec (NOVADEC pont tipo b) estadoAtualizado
            Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
    else error $ "Valor da expressão não é do mesmo tipo que a variável\nTipo esquerdo: " ++ 
                (show tipo') ++ "\nTipo direito: " ++ (show tipoExpr) ++ "\nPosição: " ++ (show posicao)
    where ((nome', tipo'):_, estadoIntermediario) = getDecs [(NOVADEC pont tipo [(VAR_COM (CRIAATRIB id expr))])] estado
          (valor, tipoExpr, estado') = evaluateExpr estadoIntermediario expr
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
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where (subprograma, estadoIntermediario) = getSubprogFromFunc func estado
          novo = addSubprograma subprograma estadoIntermediario
          posicao = getPosicaoFunc func
addSubprogs ((CRIAPROC proc):b) estado =
    case novo of
        Right estadoAtualizado -> addSubprogs b estadoAtualizado
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where (subprograma, estadoIntermediario) = getSubprogFromProc proc estado
          novo = addSubprograma subprograma estadoIntermediario
          posicao = getPosicaoProc proc
addSubprogs ((CRIAOPER oper):b) estado =
    case novo of
        Right estadoAtualizado -> addSubprogs b estadoAtualizado
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
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
    if genericLength params > 2 then 
        error $ "Muitos parâmetros para o operador: " ++ getNomeFromOp op ++ "\nPosição: " ++ (show (getPosicaoOp op))
    else if genericLength params == 2 && apenasUnario op then
        error $ "Muitos parâmetros para o operador: " ++ getNomeFromOp op ++ "\nPosição: " ++ (show (getPosicaoOp op))
    else if genericLength params == 1 && apenasBinario op then
        error $ "Poucos parâmetros para o operador: " ++ getNomeFromOp op ++ "\nPosição: " ++ (show (getPosicaoOp op))
    else if genericLength params < 1 then
        error $ "Poucos parâmetros para o operador: " ++ getNomeFromOp op ++ "\nPosição: " ++ (show (getPosicaoOp op))
    else if (getNomeFromOp op == "=" || getNomeFromOp op == "/=") && isonlyPonts decs then
        error $ "Subprograma '" ++ getNomeFromOp op ++ "' já existe com a mesma assinatura\nPosição: " ++ (show (getPosicaoOp op))
    else (Right (getNomeFromOp op, decs, stmts, getTipoFromTipoRetorno ponts tipo estadoFinal), estadoFinal)
    where
        (decs, estadoFinal) = getDecsFromParams params estado
        isonlyPonts :: [Declaracao] -> Bool
        isonlyPonts (a:b:[]) = isPont a && isPont b
        isPont :: Declaracao -> Bool
        isPont (_, TipoPonteiroFim _) = True
        isPont (_, TipoPonteiroRecursivo _) = True
        isPont _ = False

--Retorna True sse o operador pode ser apenas unário
apenasUnario :: OP -> Bool
apenasUnario (NOVONot _) = True
apenasUnario _ = False

--Retorna True sse o operador pode ser apenas binário
apenasBinario :: OP -> Bool
apenasBinario (NOVOAdd _) = True
apenasBinario (NOVOMult _) = True
apenasBinario (NOVODiv _) = True
apenasBinario (NOVOGeq _) = True
apenasBinario (NOVOLeq _) = True
apenasBinario (NOVODiff _) = True
apenasBinario (NOVOEqual _) = True
apenasBinario (NOVOGreat _) = True
apenasBinario (NOVOLess _) = True
apenasBinario _ = False

--retorna a posicao da declaracao de um operador
getPosicaoOper :: OPER -> (Int,Int)
getPosicaoOper (NOVOOPER op _ _ _ _) = getPosicaoOp op

--retorna a posicao da declaracao de um operador
getPosicaoOp :: OP -> (Int,Int)
getPosicaoOp (NOVOAdd (Add p)) = p
getPosicaoOp (NOVOSub (Sub p)) = p
getPosicaoOp (NOVOMult (Mult p)) = p
getPosicaoOp (NOVODiv (Div p)) = p
getPosicaoOp (NOVOGeq (Geq p)) = p
getPosicaoOp (NOVOLeq (Leq p)) = p
getPosicaoOp (NOVODiff (Diff p)) = p
getPosicaoOp (NOVOEqual (Equal p)) = p
getPosicaoOp (NOVOGreat (Great p)) = p
getPosicaoOp (NOVOLess (Less p)) = p
getPosicaoOp (NOVONot (Not p)) = p

--Retorna a string com o simbolo do operador
getNomeFromOp :: OP -> String
getNomeFromOp (NOVOAdd _) = "+"
getNomeFromOp (NOVOSub _) = "-"
getNomeFromOp (NOVOMult _) = "*"
getNomeFromOp (NOVODiv _) = "/"
getNomeFromOp (NOVOGeq _) = ">="
getNomeFromOp (NOVOLeq _) = "<="
getNomeFromOp (NOVODiff _) = "/="
getNomeFromOp (NOVOEqual _) = "="
getNomeFromOp (NOVOGreat _) = ">"
getNomeFromOp (NOVOLess _) = "<"
getNomeFromOp (NOVONot _) = "!"

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
    case (temSaia, temContinue) of
        (False, False) -> 
            case maybeExpr of
                Nothing -> return (removerEscopo estado1, temRetorno, temSaia, temContinue, maybeExpr, maybePos)
                otherwise -> error $ "Retorno não vazio\nPosição: " ++ (show $ fromJust maybePos) 
        (True, _) -> error $ "Comando SAIA fora de laço\nPosição: " ++ (show (fromJust maybePos))
        (_, True) -> error $ "Comando CONTINUE fora de laço\nPosição: " ++ (show (fromJust maybePos))

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

executarStmt (NOVOATRIBSTMT exprEsq (Attrib posicao) exprDir) estado0 = 
    case exprEsq of
        CRIAVAR (Var ((SingleVar (ID pos nome) _):_)) -> do
            -- Avalia o lado direito primeiro
            case evaluateExpr estado0 exprDir of
                (valorDir,tipoDir,estado1) ->
                    -- Pega a variável do lado esquerdo
                    case getVariavel nome estado1 of
                        Right (_,tipoEsq,valorEsq) ->
                            -- Calcula o valor novo
                            let (valorFinal, estado2) = assignToValue (tipoEsq,valorEsq) (tipoDir,valorDir) exprEsq posicao estado1 in
                                -- Atualiza variável
                                case atualizarVariavel (nome,tipoEsq,valorFinal) estado2 of
                                    Right estadoFinal -> return (estadoFinal, False, False, False, Nothing, Nothing)
                                    Left _ -> error $ "Variável " ++ nome ++ " não declarada\nPosição: " ++ (show pos)
                        Left _ -> error $ "Variável " ++ nome ++ " não declarada\nPosição: " ++ (show pos)
        CRIAVALOREXPR _ _ ->
             let
                --Avalia o lado direito primeiro
                (valorDir,tipoDir,estado1) = evaluateExpr estado0 exprDir
                -- Pega a variavel do lado esquerdo
                ((nome,tipoEsq,valorEsq), estado2) = getVariavelFromExpr exprEsq estado1 in
                if mesmoTipo tipoEsq tipoDir estado2 then
                    case atualizarVariavel (nome,tipoEsq,valorDir) estado2 of
                        Right estadoFinal -> return (estadoFinal, False, False, False, Nothing, Nothing)
                        Left erro -> error $ show erro ++ "\nPosição: " ++ show posicao
                else error $ "Atribuição inválida.\nTipo do lado esquerdo: " ++ show tipoEsq ++
                            "\nTipo do lado direito: " ++ show tipoDir ++ "\nPosição: " ++ show posicao
                        
                    
        otherwise -> error $ "Expressão inválida do lado esquerdo da atribuição\nPosição: " ++ (show posicao)

executarStmt (NOVOINC (CRIAINC (CRIAVAR (Var var@((SingleVar (ID pos nomeVar) _):campos ) ) ) ) ) estado0 =
    -- Busca variável
    case getVariavel nomeVar estado0 of
        Right (_, tipoVar, valorVar) ->
            -- Calcula incremento
            let (novoValor, estado1) = incrDecr (+ 1) (tipoVar, valorVar) var estado0 in
                -- Atualiza variável
                case atualizarVariavel (nomeVar, tipoVar, novoValor) estado1 of
                    Right estadoFinal -> return (estadoFinal, False, False, False, Nothing, Nothing)
                    Left _ -> error $ "Variável não declarada\nVariável: " ++ nomeVar ++ "\nPosição: " ++ (show pos) 
        Left _ -> error $ "Variável não declarada\nVariável: " ++ nomeVar ++ "\nPosição: " ++ (show pos)
        
executarStmt (NOVOINC (CRIAINC (CRIAVALOREXPR (VALOR pos) expr ) ) ) estado0 =
    -- Busca variável
    let ((nome,tipo,valor), estado1) = getVariavelFromExpr (CRIAVALOREXPR (VALOR pos) expr) estado0 in
    case valor of
        ValorInteiro v -> case atualizarVariavel (nome, tipo, ValorInteiro (v+1)) estado1 of
            Right estadoFinal -> return (estadoFinal, False, False, False, Nothing, Nothing)
            Left _ -> error $ "Acesso em posição da memória inválida:\nPosição: " ++ (show pos)
        otherwise -> error $ "Tentado incrementar/decrementar tipo não inteiro\nTipo: " ++ show tipo ++ "\nPosição: " ++ show pos

executarStmt (NOVODECR (CRIADECR (CRIAVAR (Var var@((SingleVar (ID pos nomeVar) _):campos ) ) ) ) ) estado0 =
    -- Busca variável
    case getVariavel nomeVar estado0 of
        Right (_, tipoVar, valorVar) ->
            -- Calcula incremento
            let (novoValor, estado1) = incrDecr (\x -> (x-1)) (tipoVar, valorVar) var estado0 in
                -- Atualiza variável
                case atualizarVariavel (nomeVar, tipoVar, novoValor) estado1 of
                    Right estadoFinal -> return (estadoFinal, False, False, False, Nothing, Nothing)
                    Left _ -> error $ "Variável não declarada\nVariável: " ++ nomeVar ++ "\nPosição: " ++ (show pos) 
        Left _ -> error $ "Variável não declarada\nVariável: " ++ nomeVar ++ "\nPosição: " ++ (show pos)  

executarStmt (NOVODECR (CRIADECR (CRIAVALOREXPR (VALOR pos) expr ) ) ) estado0 =
    -- Busca variável
    let ((nome,tipo,valor), estado1) = getVariavelFromExpr (CRIAVALOREXPR (VALOR pos) expr) estado0 in
    case valor of
        ValorInteiro v -> case atualizarVariavel (nome, tipo, ValorInteiro (v-1)) estado1 of
            Right estadoFinal -> return (estadoFinal, False, False, False, Nothing, Nothing)
            Left _ -> error $ "Acesso em posição da memória inválida:\nPosição: " ++ (show pos)
        otherwise -> error $ "Tentado incrementar/decrementar tipo não inteiro\nTipo: " ++ show tipo ++ "\nPosição: " ++ show pos

executarStmt (NOVOCHAMADA (CRIACHAMADA (ID posicao nome) exprs)) estado =
    case subprograma of
        Right (Left (nome, declaracoes, stmts)) ->
            let estadoFinal = foldl funcaoFold'' estadoAtualizado (zip3 (fst $ unzip declaracoes) tiposParametros valoresParametros) in
                (rodaStmts stmts estadoFinal) >>= 
                    (\(estado1, temRetorno, temSaia, temContinue, maybeExpr, maybePos) ->
                        case (temSaia, temContinue, maybeExpr) of
                            (_, _, Just _) -> error $ "Expressão retornada, incompatível com procedimento:\nPosição:" ++ (show posicao)
                            (True, _, _) -> error $ "Comando SAIA fora de laço\nPosição: " ++ (show (fromJust maybePos))
                            (_, True, _) -> error $ "Comando CONTINUE fora de laço\nPosição: " ++ (show (fromJust maybePos))
                            otherwise -> return (removerEscopo estado1, False, False, False, Nothing, Nothing))
        Right (Right (nome, declaracoes, stmts, tipoRetorno)) -> 
            error $ "Função chamada como procedimento\nPosição: " ++ (show posicao)
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where
        (valoresParametros, tiposParametros, estadoAntesDaFuncao) = evaluateExprs estado exprs
        estadoAtualizado = criarEscopo 1 estadoAntesDaFuncao
        subprograma = getSubprograma nome tiposParametros estadoAtualizado

executarStmt (NOVOSE (CRIASE token expr stmts1 (OptionalSenao stmts2))) estado =
    case res of
        ValorLogico True -> iniciaBlocoSe stmts1 estado1
        ValorLogico False -> iniciaBlocoSe stmts2 estado1
        otherwise -> error $ "Expressão não é do tipo LOGICO\nPosição: " ++ (show getPosSE)
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
            Left erro -> error $ "Delete em posição da memória inválida:\nPosição: " ++ (show $ getPosFromDelete tok)
        otherwise -> error $ "Operação de delete para expressão que não retorna Ponteiro\nTipo: " ++ 
            (show tipo) ++ "\nPosição: " ++ (show $ getPosFromDelete tok)
    where
        (val, tipo, estado1) = evaluateExpr estado expr
        justVar :: (Either ErroEstado Variavel) -> Variavel
        justVar (Right x) = x
        justVar (Left _) = error $ "Delete em posição da memória inválida:\nPosição: " ++ (show $ getPosFromDelete tok)
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
        otherwise -> error $ "Comando ESCREVA para tipo não primitivo\nTipo: " ++ (show tipo1) ++ "\nPosição: " ++ show p
    where
        (valor1, tipo1, estado1) = evaluateExpr estado expr
        showLogico :: Bool -> String
        showLogico True = "VERDADEIRO"
        showLogico False = "FALSO"

executarStmt (NOVOLEIA (CRIALEIA (LEIA p) [])) estado = return (estado, False, False, False, Nothing, Nothing)
executarStmt (NOVOLEIA (CRIALEIA (LEIA posicao) (expr:exprs))) estado0 = do
    case expr of
        CRIAVAR (Var ((SingleVar (ID pos nome) _):_)) -> do
            -- Pega a variável do lado esquerdo
            case getVariavel nome estado0 of
                Right (_,tipoEsq,valorEsq) ->
                    -- Calcula o valor novo
                    let (valorFinal, estado1) = assignToValueLeia (tipoEsq,valorEsq) expr estado0 in
                        -- Atualiza variável
                        case atualizarVariavel (nome,tipoEsq,valorFinal) estado1 of
                            Right estadoFinal -> if estadoFinal == estadoFinal then
                                    executarStmt (NOVOLEIA (CRIALEIA (LEIA posicao) (exprs))) estadoFinal
                                else error $ "Impossível"
                            Left _ -> error $ "Variável " ++ nome ++ " não declarada\nPosição: " ++ (show pos)
                Left _ -> error $ "Variável " ++ nome ++ " não declarada\nPosição: " ++ (show pos)
        CRIAVALOREXPR (VALOR pos) _ ->
                -- Pega a variavel do lado esquerdo
                let
                    ((nome,tipoEsq,valorEsq), estado1) = getVariavelFromExpr expr estado0
                    valor = unsafePerformIO (getValorLeia tipoEsq pos) in
                    case atualizarVariavel (nome,tipoEsq,valor) estado1 of
                        Right estadoFinal -> if estadoFinal == estadoFinal then
                                executarStmt (NOVOLEIA (CRIALEIA (LEIA posicao) (exprs))) estadoFinal
                            else error $ "Impossível"
                        Left erro -> error $ show erro ++ "\nPosição: " ++ show pos
        otherwise -> error $ "Expressão inválida no LEIA\nPosição: " ++ (show posicao)

executarStmt (NOVOBLOCO (CRIABLOCO stmts)) estado =
    iniciaBloco stmts estado

getVariavelFromExpr :: EXPR -> Estado -> (Variavel, Estado)
getVariavelFromExpr (CRIAVAR (Var ((SingleVar (ID posicao nome) colchetes):_))) estado = 
    case var of
        Right var' -> (var', estado)
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where var = getVariavel nome estado

getVariavelFromExpr (CRIAVALOREXPR (VALOR p) expr) estadoAntigo =
    case res of
        ValorPonteiro nome -> case var of
            Right var' -> (var', estado)
            Left erro -> error $ "Ponteiro acessado aponta para posição inválida\nPosição: " ++ (show p)
            where var = getVariavel nome estado
        otherwise -> error $ "Busca por valor em variável que não é um ponteiro\nPosição: " ++ (show p) ++ ", tipo: " ++ (show tipo)
    where
        (res, tipo, estado) = evaluateExpr estadoAntigo expr

getVarFromNome :: String -> [Variavel] -> (Maybe Variavel,Int)
getVarFromNome _ [] = (Nothing,-1)
getVarFromNome nome ((var@(nome',_,_)):vars) = if nome == nome' then (Just var,0) else (var',n+1) where (var',n) = getVarFromNome nome vars

funcaoFold'' :: Estado -> Variavel -> Estado
funcaoFold'' estado0 variavel = 
    case addVariavel variavel estado0 of
        Right estado -> estado
        Left erro -> error $ "Deu erro"

evaluateExprs :: Estado -> [EXPR] -> ([Valor],[Tipo],Estado)
evaluateExprs estado0 [] = ([], [], estado0)
evaluateExprs estado0 (expr:exprs) = (valor:valores,tipo:tipos,estadoFinal)
    where (valor, tipo, estadoIntermediario) = evaluateExpr estado0 expr
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
getposTokenOp (Not p) = p

evaluateExpr :: Estado -> EXPR -> (Valor,Tipo,Estado)

evaluateExpr estado (CRIAOU expr1 op expr2) = do
    case res1 of
        ValorLogico True -> (ValorLogico True, TipoAtomico "LOGICO", estado1)
        ValorLogico False ->
            case res2 of
                ValorLogico a -> (ValorLogico a, TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando OU:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando OU:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIASLOWOU expr1 op expr2) = do
    case res1 of
        ValorLogico a ->
            case res2 of
                ValorLogico b -> (ValorLogico (a || b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando ~OU:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando ~OU:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIAE expr1 op expr2) = do
    case res1 of
        ValorLogico False -> (ValorLogico False, TipoAtomico "LOGICO", estado1)
        ValorLogico True ->
            case res2 of
                ValorLogico a -> (ValorLogico a, TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando E:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando E:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2

evaluateExpr estado (CRIASLOWE expr1 op expr2) = do
    case res1 of
        ValorLogico a ->
            case res2 of
                ValorLogico b -> (ValorLogico (a && b), TipoAtomico "LOGICO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando ~E:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando ~E:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2


evaluateExpr estado (CRIALESS expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a < b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador <:\nTipo esquerdo: " ++ 
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a < b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador <:\nTipo esquerdo: " ++ 
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a < b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador <:\nTipo esquerdo: " ++ 
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a < b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador <:\nTipo esquerdo: " ++ 
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador <:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2
        nomeOp = "<"

evaluateExpr estado (CRIALEQ expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a <= b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador <=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a <= b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador <=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a <= b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador <=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a <= b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador <=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador <=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2
        nomeOp = "<="

evaluateExpr estado (CRIAEQUAL expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a == b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador =:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a == b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador =:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a == b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador =:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a == b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador =:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorLogico a ->
            case res2 of
                ValorLogico b -> (ValorLogico (a == b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador =:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorPonteiro a ->
            case res2 of
                ValorPonteiro b -> (ValorLogico (a == b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador =:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador =:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2
        nomeOp = "="

evaluateExpr estado (CRIAGEQ expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a >= b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador >=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a >= b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador >=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a >= b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador >=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a >= b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador >=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador >=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2
        nomeOp = ">="

evaluateExpr estado (CRIAGREAT expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a > b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador >:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a > b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador >:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a > b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador >:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a > b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador >:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador >:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2
        nomeOp = ">"

evaluateExpr estado (CRIADIFF expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorLogico (a /= b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador /=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorLogico (a /= b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador /=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorLogico (a /= b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador /=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorCaractere a ->
            case res2 of
                ValorCaractere b -> (ValorLogico (a /= b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador /=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorLogico a ->
            case res2 of
                ValorLogico b -> (ValorLogico (a /= b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador /=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorPonteiro a ->
            case res2 of
                ValorPonteiro b -> (ValorLogico (a /= b), TipoAtomico "LOGICO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador /=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador /=:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2
        nomeOp = "/="

evaluateExpr estado (CRIAADD expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (a + b), TipoAtomico "INTEIRO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador +:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a + b), TipoAtomico "REAL", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador +:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorTexto a ->
            case res2 of
                ValorTexto b -> (ValorTexto (a ++ b), TipoAtomico "TEXTO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador +:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador +:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2
        nomeOp = "+"

evaluateExpr estado (CRIASUB expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (a - b), TipoAtomico "INTEIRO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador -:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a - b), TipoAtomico "REAL", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador -:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador -:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2 
        nomeOp = "-"

evaluateExpr estado (CRIAMULT expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (a * b), TipoAtomico "INTEIRO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador *:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a * b), TipoAtomico "REAL", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador *:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador *:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2 
        nomeOp = "*"

evaluateExpr estado (CRIADIV expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (quot a b), TipoAtomico "INTEIRO", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador /:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        ValorReal a ->
            case res2 of
                ValorReal b -> (ValorReal (a / b), TipoAtomico "REAL", estado2)
                otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador /:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> case getSubprograma nomeOp [tipo1, tipo2] estado2 of
                    Right (Right func) -> rodaFuncao func estado2 [tipo1, tipo2] [res1, res2] nomeOp (getposTokenOp op)
                    otherwise -> error $ "Tipos inválidos para o operador /:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2 
        nomeOp = "/"
        
evaluateExpr estado (CRIAMOD expr1 op expr2) = do
    case res1 of
        ValorInteiro a ->
            case res2 of
                ValorInteiro b -> (ValorInteiro (mod a b), TipoAtomico "INTEIRO", estado2)
                otherwise -> error $ "Tipos inválidos para o comando MOD:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
        otherwise -> error $ "Tipos inválidos para o comando MOD:\nTipo esquerdo: " ++
                        (show tipo1) ++ "\nTipo direito: " ++ (show tipo2) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr1
        (res2,tipo2,estado2) = evaluateExpr estado1 expr2 

evaluateExpr estado (CRIANEG op expr) = do
    case res1 of
        ValorInteiro a -> (ValorInteiro (-a), TipoAtomico "INTEIRO", estado1)
        ValorReal a -> (ValorReal (-a), TipoAtomico "REAL", estado1)
        otherwise -> case getSubprograma nomeOp [tipo1] estado1 of
            Right (Right func) -> rodaFuncao func estado1 [tipo1] [res1] nomeOp (getposTokenOp op)
            otherwise -> error $ "Tipo inválido para o operador unário -:\nTipo recebido: " ++ 
                (show tipo1) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr
        nomeOp = "-"

evaluateExpr estado (CRIANOT op expr) = do
    case res1 of
        ValorLogico a -> (ValorLogico (not a), TipoAtomico "LOGICO", estado1)
        otherwise -> case getSubprograma nomeOp [tipo1] estado1 of
            Right (Right func) -> rodaFuncao func estado1 [tipo1] [res1] nomeOp (getposTokenOp op)
            otherwise -> error $ "Tipo inválido para o operador !:\nTipo recebido: " ++ 
                (show tipo1) ++ "\nPosição: " ++ show (getposTokenOp op)
    where
        (res1,tipo1,estado1) = evaluateExpr estado expr
        nomeOp = "!"

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
evaluateExpr estado (CRIANULO (NULO _)) = (valorNulo, tipoNulo, estado)
evaluateExpr estado (CRIAPARENTESES a) = evaluateExpr estado a

evaluateExpr estado (CRIAVALOREXPR (VALOR p) expr) = 
    case val of
        ValorPonteiro s -> (getValor $ getVariavel s estado1, getTipoApontado tipo, estado1)
        otherwise -> error $ "Busca por valor em variável que não é um ponteiro:\nTipo: " ++ (show tipo) ++ "\nPosição: " ++ (show p)
    where 
        (val, tipo, estado1) = evaluateExpr estado expr
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
        Right (_, tt, valor_estr) -> 
            case traduzTipo tt estado of
                TipoEstrutura nome_estr _ ->
                    case evaluateEstr estado valor_estr snglVars of
                        Right result -> result
                        Left erro -> error $ nome_estr ++ " " ++ (show erro) ++ "\nPosição: " ++ (show posicao)
                _ -> error $ "Variável " ++ nome ++ " não é uma estrutura\nPosição: " ++ (show posicao)
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
        Right (Right func@(nome, declaracoes, stmts, tipoRetorno)) ->
            rodaFuncao func estadoAtualizado tiposParametros valoresParametros nome posicao
        Left erro -> error $ (show erro) ++ "\nPosição: " ++ (show posicao)
    where
        (valoresParametros, tiposParametros, estadoAtualizado) = evaluateExprs estado exprs
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
        Just (_, tt, valor_estr) -> case traduzTipo tt estado of
            TipoEstrutura _ _ -> evaluateEstr estado valor_estr snglVars
            _ -> error $ nome ++ " não é uma estrutura\nPosição: " ++ (show posicao)
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
        (ValorVetor _, _, _)     -> Left "Vetor passado como subscrito de outro vetor"
        (ValorPonteiro _, p, _)  -> Left $ (show p) ++ " passado como subscrito de outro vetor"
        (ValorEstrutura _, tipo, _) -> Left $ "ESTRUTURA (" ++ (show tipo) ++ ") passada como subscrito de vetor"
    where
        res_expr = evaluateExpr estado expr

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

--Executa uma função e devolve o (valor,tipo,estado) após a execução
{-Recebe:
    a função, 
    o estado, 
    os tipos dos parametros (já conferidos), 
    os valores reais, 
    nome da função
    a posição em que a função foi chamada (para erros)
-}
rodaFuncao :: Funcao -> Estado -> [Tipo] -> [Valor] -> String -> (Int,Int) -> (Valor, Tipo, Estado)
rodaFuncao (_, declaracoes, stmts, _) estado tiposParametros valoresParametros nome posicao =
    let
        estadoAtualizado = criarEscopo 1 estado
        estadoFinal = foldl funcaoFold'' estadoAtualizado (zip3 (fst $ unzip declaracoes) tiposParametros valoresParametros) in
                unsafePerformIO ((rodaStmts stmts estadoFinal) >>= 
                    (\(estado1, temRetorno, temSaia, temContinue, maybeExpr, maybePos) ->
                        case (temSaia, temContinue, maybeExpr) of
                            (_, _, Nothing) -> error $ "Função/Operador retornou sem valor\nNome: " ++ (show nome) ++
                                "\nPosição: " ++ show posicao
                            (True, _, _) -> error $ "Comando SAIA fora de laço\nPosição: " ++ (show (fromJust maybePos))
                            (_, True, _) -> error $ "Comando CONTINUE fora de laço\nPosição: " ++ (show (fromJust maybePos))
                            otherwise -> ((return (evaluateExpr estado1 (fromJust maybeExpr))) >>=
                                (\(valor, tipo, estado2) -> return (valor, tipo, removerEscopo estado2))) ))

-- Inicio das funções auxiliares para leitura

assignToValueLeia :: (Tipo, Valor) -> EXPR -> Estado -> (Valor, Estado)
assignToValueLeia (tipoEsq, valorEsq) expr estadoAtual =
    case expr of
        -- Variável comum
        CRIAVAR (Var [SingleVar (ID posicao _) (OptionalSQBrack [])]) ->
            -- Caso o tipo esquerdo seja passivo de leitura
            let valor = unsafePerformIO (getValorLeia tipoEsq posicao) in
                (valor, estadoAtual)
        
        -- Vetor
        CRIAVAR (Var ((SingleVar (ID posicao nomeVar) (OptionalSQBrack (id_expr:ids))):campos)) ->
            case tipoEsq of
                -- Se o lado esquerdo for do tipo vetor
                TipoVetor (dim:dims) tipoEleVet ->
                    case valorEsq of
                        -- Se o valor esquerdo casa com ValorVetor
                        ValorVetor valorVet ->
                            case getIth valorVet id of
                                -- Pega o Ith
                                Right ith ->
                                    -- Se for unidimensional
                                    if null ids then
                                        if not (null dims) then error $ "Número de índices menor que o número de dimensões do vetor\nVariável: " ++ nomeVar ++ "\nPosição: " ++ (show posicao)
                                        -- Chama a recursão
                                        else let (ithAtualizado, estadoAtualizado2) = assignToValueLeia (tipoEleVet, ith) (CRIAVAR (Var ((SingleVar (ID posicao nomeVar) (OptionalSQBrack ids)):campos))) estadoAtualizado1 in
                                            -- Substitui o valor atualizado
                                            case setIth ithAtualizado id valorVet of
                                                Right valorAtualizado -> (ValorVetor valorAtualizado, estadoAtualizado2)
                                                Left err -> error $ (show err) ++ "\nPosição: " ++ (show posicao)
                                    -- Se for multidimensional
                                    else
                                        -- Chama a recursão
                                        let (ithAtualizado, estadoAtualizado2) = assignToValueLeia (TipoVetor dims tipoEleVet, ith) (CRIAVAR (Var ((SingleVar (ID posicao nomeVar) (OptionalSQBrack ids)):campos))) estadoAtualizado1 in
                                            -- Substitui o valor atualizado
                                            case setIth ithAtualizado id valorVet of
                                                Right valorAtualizado -> (ValorVetor valorAtualizado, estadoAtualizado2)
                                                Left err -> error $ (show err) ++ "\nPosição: " ++ (show posicao)
                                Left err -> error $ (show err) ++ "\nPosição: " ++ (show posicao)
                            where
                                (id, estadoAtualizado1) = 
                                    case evaluateExpr estadoAtual id_expr of
                                        (ValorInteiro valor, TipoAtomico "INTEIRO", est) -> (valor, est)
                                        otherwise -> error $ "Expressão não inteira fornecida como id de vetor\nPosição: " ++ (show posicao)
                        otherwise -> error $ "Tentando acessar índice de variável que não é um vetor\nVariável " ++ nomeVar ++ " é do tipo " ++ (show tipoEsq) ++ "\nPosição: " ++ (show posicao)
                TipoVetor [] tipoEleVet -> error $ "Número de índices maior que o número de dimensões do vetor\nVariável: " ++ nomeVar ++ "\nPosição: " ++ (show posicao)
                otherwise -> error $ "Tentando acessar índice de variável que não é um vetor\nVariável " ++ nomeVar ++ " é do tipo " ++ (show tipoEsq) ++ "\nPosição: " ++ (show posicao)
                

        -- Estrutura
        CRIAVAR ( Var ((SingleVar (ID posVarEstr nomeVarEstr) (OptionalSQBrack [])):campos@((SingleVar (ID posCampo nomeCampo) _):_) ) ) ->
            -- Verifica se a cabeça da lista é uma estrutura
            case traduzTipo tipoEsq estadoAtual of
                -- Se for uma estrutura
                TipoEstrutura nomeEstr decs ->
                    -- Se o valor esquerdo casa com ValorEstrutura
                    case valorEsq of
                        ValorEstrutura varsEstr ->
                            -- Pega o tipo e valor correspondentes ao primeiro campo
                            case getCampo nomeCampo varsEstr of
                                Right valorCampo -> 
                                    -- Chama recursivamente paro o valorCampo
                                    let (valorAtualizado, estadoAtualizado) = assignToValueLeia valorCampo (CRIAVAR (Var campos)) estadoAtual in
                                        -- Substitui o valor atualizado do campo no campo correspondente da estrutura
                                        case setCampo nomeCampo valorAtualizado varsEstr of
                                            Right varsEstrAtualizadas -> (ValorEstrutura varsEstrAtualizadas, estadoAtualizado)
                                            Left err -> error $ err ++ " em " ++ nomeVarEstr ++ ", que é do tipo " ++ (show tipoEsq) ++ "\nPosição: " ++ (show posCampo)
                                Left err -> error $ err ++ " em " ++ nomeVarEstr ++ ", que é do tipo " ++ (show tipoEsq) ++ "\nPosição: " ++ (show posCampo)
                        otherwise -> error $ "Tentando acessar campos de uma variável que não é estrutura\nVariável " ++ nomeVarEstr ++ " é do tipo " ++ (show tipoEsq) ++ "\nPosição: " ++ (show posVarEstr)
                otherwise -> error $ "Tentando acessar campos de uma variável que não é estrutura\nVariável " ++ nomeVarEstr ++ " é do tipo " ++ (show tipoEsq) ++ "\nPosição: " ++ (show posVarEstr)


getValorLeia :: Tipo -> (Int, Int) -> IO Valor
getValorLeia tipo p =
    case tipo of
        TipoAtomico "INTEIRO" -> do
            hFlush stdout
            s <- getPalavra
            case readMaybe s :: Maybe Integer of
                Just i -> return $ ValorInteiro i
                Nothing -> error $ "Valor não permitido como INTEIRO\nPosição: " ++ (show p)
        TipoAtomico "REAL" -> do
            hFlush stdout
            s <- getPalavra
            case readMaybe s :: Maybe Double of
                Just i -> return $ ValorReal i
                Nothing -> error $ "Valor não permitido como REAL\nPosição: " ++ (show p)
        TipoAtomico "CARACTERE" -> do
            hFlush stdout
            c <- getChar
            return $ ValorCaractere c
        TipoAtomico "TEXTO" -> do
            hFlush stdout
            s <- getPalavra
            return $ ValorTexto s
        TipoAtomico "LOGICO" -> do
            hFlush stdout
            s <- getPalavra
            case s of
                "VERDADEIRO" -> return $ ValorLogico True
                "FALSO" -> return $ ValorLogico False
                otherwise -> error $ "Valor não permitido como LOGICO\nPosição: " ++ (show p)
        otherwise -> error $ "Comando LEIA para tipo não primitivo\nTipo: " ++ (show tipo) ++ "\nPosição: " ++ show p
-- Fim das funções auxiliares para leitura

-- Início de funções auxiliares para a atribução ----------------------------------------------------------

assignToValue :: (Tipo, Valor) -> (Tipo, Valor) -> EXPR -> (Int,Int) -> Estado -> (Valor, Estado)
assignToValue (tipoEsq, valorEsq) (tipoDir, valorDir) expr posicao estadoAtual =
    case expr of
        -- Variável comum
        CRIAVAR (Var [SingleVar (ID pos _) (OptionalSQBrack [])]) ->
            -- Caso o tipo esquerdo seja passivo de atribuição
            case tipoEsq of
                TipoVetor _ _ -> error $ "Não é possível atribuir valores ao tipo " ++ (show tipoEsq) ++ "\nVetores não podem receber atribuições\nPosição: " ++ (show pos)
                otherwise ->
                    if mesmoTipo tipoEsq tipoDir estadoAtual then
                        (valorDir, estadoAtual)
                    else
                        error $ "Tipos incompatíveis na atribuição.\nTipo esperado: " ++ (show tipoEsq) ++ "\nTipo recebido: " ++ (show tipoDir) ++ "\nPosição: " ++ (show posicao)
        
        -- Vetor
        CRIAVAR (Var ((SingleVar (ID pos nomeVar) (OptionalSQBrack (id_expr:ids))):campos)) ->
            case tipoEsq of
                -- Se o lado esquerdo for do tipo vetor
                TipoVetor (dim:dims) tipoEleVet ->
                    case valorEsq of
                        -- Se o valor esquerdo casa com ValorVetor
                        ValorVetor valorVet ->
                            case getIth valorVet id of
                                -- Pega o Ith
                                Right ith ->
                                    -- Se for unidimensional
                                    if null ids then
                                        if not (null dims) then error $ "Número de índices menor que o número de dimensões do vetor\nVariável: " ++ nomeVar ++ "\nPosição: " ++ (show pos)
                                        -- Chama a recursão
                                        else let (ithAtualizado, estadoAtualizado2) = assignToValue (tipoEleVet, ith) (tipoDir,valorDir) (CRIAVAR (Var ((SingleVar (ID pos nomeVar) (OptionalSQBrack ids)):campos))) posicao estadoAtualizado1 in
                                            -- Substitui o valor atualizado
                                            case setIth ithAtualizado id valorVet of
                                                Right valorAtualizado -> (ValorVetor valorAtualizado, estadoAtualizado2)
                                                Left err -> error $ (show err) ++ "\nPosição: " ++ (show pos)
                                    -- Se for multidimensional
                                    else
                                        -- Chama a recursão
                                        let (ithAtualizado, estadoAtualizado2) = assignToValue (TipoVetor dims tipoEleVet, ith) (tipoDir,valorDir) (CRIAVAR (Var ((SingleVar (ID pos nomeVar) (OptionalSQBrack ids)):campos))) posicao estadoAtualizado1 in
                                            -- Substitui o valor atualizado
                                            case setIth ithAtualizado id valorVet of
                                                Right valorAtualizado -> (ValorVetor valorAtualizado, estadoAtualizado2)
                                                Left err -> error $ (show err) ++ "\nPosição: " ++ (show pos)
                                Left err -> error $ (show err) ++ "\nPosição: " ++ (show pos)
                            where
                                (id, estadoAtualizado1) = 
                                    case evaluateExpr estadoAtual id_expr of
                                        (ValorInteiro valor, TipoAtomico "INTEIRO", est) -> (valor, est)
                                        otherwise -> error $ "Expressão não inteira fornecida como id de vetor\nPosição: " ++ (show pos)
                        otherwise -> error $ "Tentando acessar índice de variável que não é um vetor\nVariável " ++ nomeVar ++ " é do tipo " ++ (show tipoEsq) ++ "\nPosição: " ++ (show pos)
                TipoVetor [] tipoEleVet -> error $ "Número de índices maior que o número de dimensões do vetor\nVariável: " ++ nomeVar ++ "\nPosição: " ++ (show pos)
                otherwise -> error $ "Tentando acessar índice de variável que não é um vetor\nVariável " ++ nomeVar ++ " é do tipo " ++ (show tipoEsq) ++ "\nPosição: " ++ (show pos)
                

        -- Estrutura
        CRIAVAR ( Var ((SingleVar (ID posVarEstr nomeVarEstr) (OptionalSQBrack [])):campos@((SingleVar (ID posCampo nomeCampo) _):_) ) ) ->
            -- Verifica se a cabeça da lista é uma estrutura
            case traduzTipo tipoEsq estadoAtual of
                -- Se for uma estrutura
                TipoEstrutura nomeEstr decs ->
                    -- Se o valor esquerdo casa com ValorEstrutura
                    case valorEsq of
                        ValorEstrutura varsEstr ->
                            -- Pega o tipo e valor correspondentes ao primeiro campo
                            case getCampo nomeCampo varsEstr of
                                Right valorCampo -> 
                                    -- Chama recursivamente paro o valorCampo
                                    let (valorAtualizado, estadoAtualizado) = assignToValue valorCampo (tipoDir,valorDir) (CRIAVAR (Var campos)) posicao estadoAtual in
                                        -- Substitui o valor atualizado do campo no campo correspondente da estrutura
                                        case setCampo nomeCampo valorAtualizado varsEstr of
                                            Right varsEstrAtualizadas -> (ValorEstrutura varsEstrAtualizadas, estadoAtualizado)
                                            Left err -> error $ err ++ " em " ++ nomeVarEstr ++ ", que é do tipo " ++ (show tipoEsq) ++ "\nPosição: " ++ (show posCampo)
                                Left err -> error $ err ++ " em " ++ nomeVarEstr ++ ", que é do tipo " ++ (show tipoEsq) ++ "\nPosição: " ++ (show posCampo)
                        otherwise -> error $ "Tentando acessar campos de uma variável que não é estrutura\nVariável " ++ nomeVarEstr ++ " é do tipo " ++ (show tipoEsq) ++ "\nPosição: " ++ (show posVarEstr)
                otherwise -> error $ "Tentando acessar campos de uma variável que não é estrutura\nVariável " ++ nomeVarEstr ++ " é do tipo " ++ (show tipoEsq) ++ "\nPosição: " ++ (show posVarEstr)


-- Fornece o nome da variável e uma lista de variáveis, e retorna o valor e o tipo da variável com o nome fornecido
getCampo :: String -> [Variavel] -> Either String (Tipo,Valor)
getCampo nome [] = Left $ "Campo " ++ nome ++ " não encontrado"
getCampo nome ((nomeCampo,tipo,valor):campos)
    | nome == nomeCampo = Right (tipo, valor)
    | otherwise         = getCampo nome campos

-- Recebe nome do campo, o novo valor e a lista de variáveis, e retorna a lista de variáveis atualizada
setCampo :: String -> Valor -> [Variavel] -> Either String [Variavel]
setCampo nome _ [] = Left $ "Campo " ++ nome ++ " não encontrado"
setCampo nome novo ((nomeCampo,tipo,valor):campos)
    | nome == nomeCampo = Right ((nomeCampo,tipo,novo):campos)
    | otherwise =
        case setCampo nome novo campos of
            Right result -> Right ((nomeCampo,tipo,valor):result)
            Left err -> Left err

-- Retorna o i-ésimo elemento de uma lista
getIth :: [t] -> Integer -> Either String t
getIth [] _ = Left "Índices fora de faixa"
getIth (a:b) i
    | i < 1     = Left "Índices fora de faixa" 
    | i == 1    = Right a
    | otherwise = getIth b (i-1)

-- Substitui o primeiro parâmetro na posição dada pelo segundo parâmetro na lista de valores
setIth :: t -> Integer -> [t] -> Either String [t]
setIth val id [] = Left "Índices fora de faixa"
setIth val id (head:tail)
    | id < 1 = Left "Índices fora de faixa"
    | id == 1 = Right $ val:tail
    | id > 1 = 
        case setIth val (id-1) tail of
            Right res -> Right (head:res)
            Left err -> Left err
-- Fim de funções auxiliares para a atribução -------------------------------------------------------------

-- Início das funções auxilires para Incremento/Decremento -------------------------------------------------------------
incrDecr :: (Integer -> Integer) -> (Tipo,Valor) -> [SingleVAR] -> Estado -> (Valor,Estado)
-- Variável comum
incrDecr func (tipo, valor) [SingleVar (ID pos nome) (OptionalSQBrack [])] estado =
    case (tipo, valor) of
        -- Testa o tipo
        (TipoAtomico "INTEIRO", ValorInteiro x) -> (ValorInteiro (func x), estado) 
        otherwise -> error $ "Tentado incrementar/decrementar tipo não inteiro\nTipo: " ++ (show tipo) ++ "\nPosição: " ++ (show pos)

-- Vetor
incrDecr func (tipo, valor) ((SingleVar (ID pos nome) (OptionalSQBrack (id_expr:ids))):campos) estado =
    case (tipo, valor) of
        ( TipoVetor (dim:dims) tipoEleVetor, ValorVetor valorVet ) ->
            -- pego o ith elemento do vetor
            case getIth valorVet id of
                Right ith ->
                    -- Se for unidimensional
                    if null ids then
                        if null dims then
                            -- Chama a recursão
                            let (ithAtualizado, estadoAtualizado2) = incrDecr func (tipoEleVetor, ith) ((SingleVar (ID pos nome) (OptionalSQBrack ids)):campos) estadoAtualizado1 in
                                -- Substitui o valor atualizado
                                case setIth ithAtualizado id valorVet of
                                    Right valorAtualizado -> (ValorVetor valorAtualizado, estadoAtualizado2)
                                    Left err -> error $ (show err) ++ "\nPosição: " ++ (show pos)
                        else
                            error $ "Número de índices menor que o número de dimensões do vetor\nVariável: " ++ nome ++ "\nPosição: " ++ (show pos)
                    -- Se for multidimensional
                    else
                        -- Chama a recursão
                            let (ithAtualizado, estadoAtualizado2) = incrDecr func (TipoVetor dims tipoEleVetor, ith) ((SingleVar (ID pos nome) (OptionalSQBrack ids)):campos) estadoAtualizado1 in
                                -- Substitui o valor atualizado
                                case setIth ithAtualizado id valorVet of
                                    Right valorAtualizado -> (ValorVetor valorAtualizado, estadoAtualizado2)
                                    Left err -> error $ (show err) ++ "\nPosição: " ++ (show pos)
                Left err -> error $ (show err) ++ "\nPosição: " ++ (show pos)
            where
                (id, estadoAtualizado1) = 
                    case evaluateExpr estado id_expr of
                        (ValorInteiro valor, TipoAtomico "INTEIRO", est) -> (valor, est)
                        otherwise -> error $ "Expressão não inteira fornecida como id de vetor\nPosição: " ++ (show pos)
        otherwise -> error $ "Tentando acessar índice de variável que não é um vetor\nVariável: " ++ nome ++ "\nTipo: " ++ (show tipo) ++ "\nPosição: " ++ (show pos)

-- Estrutura
incrDecr func (tipo, valor) ((SingleVar (ID posVarEstr nomeVarEstr) (OptionalSQBrack [])):campos@((SingleVar (ID posCampo nomeCampo) _):_) ) estado =
    -- Verifica se a cabeça da lista é uma estrutura
    case (traduzTipo tipo estado,valor) of
        -- Se for uma estrutura
        (TipoEstrutura nomeEstr decs, ValorEstrutura varsEstr) ->
            -- Pega o tipo e valor correspondentes ao primeiro campo
            case getCampo nomeCampo varsEstr of
                Right valorCampo -> 
                    -- Chama recursivamente paro o valorCampo
                    let (valorAtualizado, estadoAtualizado) = incrDecr func valorCampo campos estado in
                        -- Substitui o valor atualizado do campo no campo correspondente da estrutura
                        case setCampo nomeCampo valorAtualizado varsEstr of
                            Right varsEstrAtualizadas -> (ValorEstrutura varsEstrAtualizadas, estadoAtualizado)
                            Left err -> error $ err ++ " em " ++ nomeVarEstr ++ ", que é do tipo " ++ (show tipo) ++ "\nPosição: " ++ (show posCampo)
                Left err -> error $ err ++ " em " ++ nomeVarEstr ++ ", que é do tipo " ++ (show tipo) ++ "\nPosição: " ++ (show posCampo)
        otherwise -> error $ "Tentando acessar campos de uma variável que não é estrutura\nVariável " ++ nomeVarEstr ++ " é do tipo " ++ (show tipo) ++ "\nPosição: " ++ (show posVarEstr)

-- Fim das funções auxilires para Incremento/Decremento ----------------------------------------------------------------
