module Estado (
    Estado,
    Escopo,
    Funcao,
    ErroEstado (..),
    criarEscopo,
    getEscopoById,
    getEscopoAtual,
    getIdEscopoAtual,
    removerEscopo,
    addVariavel,
    getVariavel,
    atualizarVariavel,
    addTipo,
    addFuncao
) where

import Tipos
import Data.Maybe
import Data.Either
import Data.List

-- pilha de escopos, lista de tipos
type Estado = ([Escopo], [Tipo], [Funcao])

-- Número do Escopo, Número do escopo anterior, Tabela de declaracoes
type Escopo = (Integer, Integer, [Variavel])

-- Nome, Parametros, Tipo retorno
type Funcao = (String, [Declaracao], Tipo)

data ErroEstado = ErroNomeDuplicado String
                | ErroTipoDuplicado String
                | ErroFuncaoDuplicada String
                | ErroBuscaVariavel String
                | ErroFuncaoNaoEncontrada String
                deriving (Show)

{- Parâmetros:
	Integer -> Id do Escopo pai
	Estado  -> Estado atual do programa
-}
criarEscopo :: Integer -> Estado -> Estado
criarEscopo idEscopoAtual (pilhaEscopo, tipos, funcoes) =
        (((genericLength pilhaEscopo) + 1, idEscopoAtual, []):pilhaEscopo, tipos, funcoes)

{- Parâmetros:
	Integer -> Id do Escopo a ser procurado
	Estado  -> Estado atual do programa
-}
getEscopoById :: Integer -> Estado -> Escopo
getEscopoById idEscopoAtual (pilhaEscopo, _, _) = fromJust $ getEscopoByIdFromPilha idEscopoAtual pilhaEscopo

-- Função auxiliar para getEscopoById
getEscopoByIdFromPilha :: Integer -> [Escopo] -> Maybe Escopo
getEscopoByIdFromPilha _        []    = Nothing
getEscopoByIdFromPilha idEscopo pilha = Just $ genericIndex pilha idEscopo

-- Retorna o escopo atual a partir do estado
getEscopoAtual :: Estado -> Escopo
getEscopoAtual (pilhaEscopo, _, _) = head pilhaEscopo

-- Retorna o Id do escopo atual
getIdEscopoAtual :: Estado -> Integer
getIdEscopoAtual estado = idEscopoAtual where (idEscopoAtual, _, _) = getEscopoAtual estado

-- Remove o escopo do topo da pilha
removerEscopo :: Estado -> Estado
removerEscopo ((escopo:pilhaEscopo), tipos, funcoes) = (pilhaEscopo, tipos, funcoes)
    
-- Adicionar símbolo ao estado
addVariavel :: Variavel -> Estado -> Either ErroEstado Estado
addVariavel simbolo estado@(escopos, tipos, funcoes) = 
    case escopo of
        Right escopoAtualizado -> Right $ (escopoAtualizado: tail escopos, tipos, funcoes)
        Left erro -> Left erro
    where escopo = addVariavelEscopo simbolo (getEscopoAtual estado)

-- Adicionar símbolo ao escopo atual
addVariavelEscopo :: Variavel -> Escopo -> Either ErroEstado Escopo
addVariavelEscopo simbolo (idEscopo, idEscopoAnterior, tabelaAtual) = 
    case tabela of 
        Right tabelaAtualizada -> Right (idEscopo, idEscopoAnterior, tabelaAtualizada)
        Left erro -> Left erro
    where tabela = addVariavelTabela simbolo tabelaAtual

-- Adicionar simbolo à tabela de símbolos
addVariavelTabela :: Variavel -> [Variavel] -> Either ErroEstado [Variavel] 
addVariavelTabela (nome, _, _) tabela =
    case getVariavelTabela nome tabela of
        Just simbolo -> Right $ simbolo:tabela
        Nothing -> Left $ ErroNomeDuplicado ("Já existe uma variável com o nome:" ++ nome)

-- Busca por um símbolo na tabela de símbolos pelo nome
getVariavelTabela :: String -> [Variavel] -> Maybe Variavel
getVariavelTabela _    [] = Nothing
getVariavelTabela nomeNovo (simbolo@(nome, _, _):tabela) =
    if nomeNovo == nome then
        Just simbolo
    else
        getVariavelTabela nomeNovo tabela

-- Busca por um símbolo no estado pelo nome
getVariavel :: String -> Estado -> Either ErroEstado Variavel
getVariavel nome (pilhaEscopo, _, _) = getVariavelPilha nome pilhaEscopo

-- Busca por um síbolo na pilha de escopos pelo nome
getVariavelPilha :: String -> [Escopo] -> Either ErroEstado Variavel
getVariavelPilha nome pilha =
    case simbolo of
        Just simbolo' -> Right simbolo'
        Nothing -> Left $ ErroBuscaVariavel $ "Nome '" ++ nome ++ "' não encontrado na tabela de símbolos"
    where simbolo = getVariavelPilha' nome (Just (head pilha)) pilha

-- Auxiliar para a busca por um símbolo na pilha
getVariavelPilha' :: String -> Maybe Escopo -> [Escopo] -> Maybe Variavel
getVariavelPilha' _ Nothing _ = Nothing
getVariavelPilha' nome (Just (_, idEscopoAnterior, tabelaVariavels)) pilha =
    case simbolo of
        Just simbolo' -> Just simbolo'
        Nothing -> getVariavelPilha' nome (getEscopoByIdFromPilha idEscopoAnterior pilha) pilha
    where simbolo = getVariavelTabela nome tabelaVariavels

-- Atualiza um símbolo no estado passado
atualizarVariavel :: Variavel -> Estado -> Either ErroEstado Estado
atualizarVariavel simbolo (pilhaAtual, tipos, funcoes) =
    case pilhaAtualizada of
        Right pilhaAtualizada -> Right $ (pilhaAtualizada, tipos, funcoes)
        Left error -> Left error
    where pilhaAtualizada = atualizarVariavelPilha simbolo pilhaAtual

-- Atualiza um símbolo no pilha de escopos passada
atualizarVariavelPilha :: Variavel -> [Escopo] -> Either ErroEstado [Escopo]
atualizarVariavelPilha simbolo@(nome, _, _) pilhaAtual = 
    case pilha of
        Just pilhaAtualizada -> Right pilhaAtualizada
        Nothing -> Left $ ErroBuscaVariavel $ "Nome '" ++ nome ++ "' não encontrado na tabela de símbolos"
    where pilha = atualizarVariavelPilha' simbolo (Just (head pilhaAtual)) pilhaAtual

-- Auxiliar para a atualização do símbolo na pilha
atualizarVariavelPilha' :: Variavel -> Maybe Escopo -> [Escopo] -> Maybe [Escopo]
atualizarVariavelPilha' _ Nothing _ = Nothing
atualizarVariavelPilha' simbolo (Just escopoAtual@(idEscopoAtual, idEscopoAnterior, tabelaAtual)) pilha =
    case tabela of
        Just tabelaAtualizada -> Just $ inicio ++ [(idEscopoAtual, idEscopoAnterior, tabelaAtualizada)] ++ tail fim
        Nothing -> atualizarVariavelPilha' simbolo (getEscopoByIdFromPilha idEscopoAnterior pilha) pilha
    where tabela = atualizarVariavelTabela simbolo tabelaAtual
          (inicio, fim) = genericSplitAt idEscopoAtual pilha

-- Atualiza um símbolo na tabela de símbolos
atualizarVariavelTabela :: Variavel -> [Variavel] -> Maybe [Variavel]
atualizarVariavelTabela _ [] = Nothing
atualizarVariavelTabela simbolo@(nome, _, _) (simboloAtual@(nome', _, _):tabelaAtual) =
    if nome == nome' then
        Just $ simbolo:tabelaAtual
    else
        case tabela of
            Just tabelaAtualizada -> Just $ simboloAtual:tabelaAtualizada
            Nothing -> Nothing
        where tabela = atualizarVariavelTabela simbolo tabelaAtual

addTipo :: Tipo -> Estado -> Either ErroEstado Estado
addTipo tipo (pilha, tiposAtuais, funcoes) =
    case tipos of
        Right tiposAtualizados -> Right $ (pilha, tiposAtualizados, funcoes)
        Left error -> Left error
    where tipos = addTipoLista tipo tiposAtuais

addTipoLista :: Tipo -> [Tipo] -> Either ErroEstado [Tipo]
addTipoLista tipo tipos =
    if notElem tipo tipos then
        Right $ tipo:tipos
    else
        Left $ ErroTipoDuplicado $ "Tipo '" ++ show tipo ++ "' já foi declarado anteriormente"

addFuncao :: Funcao -> Estado -> Either ErroEstado Estado
addFuncao funcao (pilha, tipos, funcoesAtuais) =
    case funcoes of
        Right funcoesAtualizadas -> Right (pilha, tipos, funcoesAtualizadas)
        Left error -> Left error
    where funcoes = addFuncaoLista funcao funcoesAtuais

addFuncaoLista :: Funcao -> [Funcao] -> Either ErroEstado [Funcao]
addFuncaoLista funcao [] = Right [funcao]
addFuncaoLista funcao@(nome, parametros, tipoRetorno) (funcao'@(nome', parametros', tipoRetorno'):funcoesAtuais) =
    if nome == nome' && tiposParametros == tiposParametros' then
        Left $ ErroFuncaoDuplicada $ "Função '" ++ nome ++ "' já foi criada do jeito informado"
    else
        let funcoes = addFuncaoLista funcao funcoesAtuais in
        case funcoes of
            Right funcoesAtualizadas -> Right $ funcao':funcoesAtualizadas
            Left erro -> Left erro
    where tiposParametros  = snd $ unzip parametros
          tiposParametros' = snd $ unzip parametros'

getFuncao :: String -> Estado -> Either ErroEstado Funcao
getFuncao nomeFuncao (_, _, funcoes) = getFuncaoLista nomeFuncao funcoes

getFuncaoLista :: String -> [Funcao] -> Either ErroEstado Funcao
getFuncaoLista nome [] = Left $ ErroFuncaoNaoEncontrada $ "Função '" ++ nome ++ "' não encontrada"
getFuncaoLista nome (funcao@(nome', _, _):funcoes) =
    if nome == nome' then
        Right funcao
    else
        getFuncaoLista nome funcoes