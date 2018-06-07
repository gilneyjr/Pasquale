module Estado (
    Estado,
    Escopo,
    Declaracao,
    Funcao,
    Tipo (..),
    Valor (..),
    ErroEstado (..),
    criarEscopo,
    getEscopoById,
    getEscopoAtual,
    getIdEscopoAtual,
    removerEscopo,
    addDeclaracao,
    getDeclaracao,
    atualizarDeclaracao
) where

import Arvore
import Data.Maybe
import Data.Either
import Data.List

-- pilha de escopos, lista de tipos
type Estado = ([Escopo], [Tipo], [Funcao])

-- Número do Escopo, Número do escopo anterior, Tabela de declaracoes
type Escopo = (Integer, Integer, [Declaracao])

-- Nome, Tipo, Valor
type Declaracao = (String, Tipo, Valor)

-- Nome, Parametros, Tipo retorno
type Funcao = (String, [Declaracao], Tipo)

data Tipo = TipoAtomico String
          | TipoVetor Tipo
          | TipoPonteiro Tipo
          | TipoEstrutura String [Tipo]
          deriving (Show, Eq)

data Valor = ValorInteiro Integer
           | ValorLogico Bool
           | ValorTexto String
           | ValorCaractere Char
           | ValorReal Double
           | ValorVetor [Valor]
           | ValorEstrutura [Valor]
           deriving (Show, Eq)

data ErroEstado = ErroNomeDuplicado String
                | ErroBuscaDeclaracao String
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
getEscopoAtual (pilhaEscopo, _, _)= head pilhaEscopo

-- Retorna o Id do escopo atual
getIdEscopoAtual :: Estado -> Integer
getIdEscopoAtual estado = idEscopoAtual where (idEscopoAtual, _, _) = getEscopoAtual estado

-- Remove o escopo do topo da pilha
removerEscopo :: Estado -> Estado
removerEscopo ((escopo:pilhaEscopo), tipos, funcoes) = (pilhaEscopo, tipos, funcoes)

-- Adicionar símbolo ao estado
addDeclaracao :: Declaracao -> Estado -> Either ErroEstado Estado
addDeclaracao simbolo estado@(escopos, tipos, funcoes) = 
        case escopo of
                Right escopoAtualizado -> Right $ (escopoAtualizado: tail escopos, tipos, funcoes)
                Left erro -> Left erro
        where escopo = addDeclaracaoEscopo simbolo (getEscopoAtual estado)

-- Adicionar símbolo ao escopo atual
addDeclaracaoEscopo :: Declaracao -> Escopo -> Either ErroEstado Escopo
addDeclaracaoEscopo simbolo (idEscopo, idEscopoAnterior, tabelaAtual) = 
        case tabela of 
                Right tabelaAtualizada -> Right (idEscopo, idEscopoAnterior, tabelaAtualizada)
                Left erro -> Left erro
        where tabela = addDeclaracaoTabela simbolo tabelaAtual

-- Adicionar simbolo à tabela de símbolos
addDeclaracaoTabela :: Declaracao -> [Declaracao] -> Either ErroEstado [Declaracao] 
addDeclaracaoTabela (nome, _, _) tabela =
        case getDeclaracaoTabela nome tabela of
                Just simbolo -> Right $ simbolo:tabela
                Nothing -> Left $ ErroNomeDuplicado ("Já existe uma variável com o nome:" ++ nome)

-- Busca por um símbolo na tabela de símbolos pelo nome
getDeclaracaoTabela :: String -> [Declaracao] -> Maybe Declaracao
getDeclaracaoTabela _    [] = Nothing
getDeclaracaoTabela nomeNovo (simbolo@(nome, _, _):tabela) =
        if nomeNovo == nome then
                Just simbolo
        else
                getDeclaracaoTabela nomeNovo tabela

-- Busca por um símbolo no estado pelo nome
getDeclaracao :: String -> Estado -> Either ErroEstado Declaracao
getDeclaracao nome (pilhaEscopo, _, _) = getDeclaracaoPilha nome pilhaEscopo

-- Busca por um síbolo na pilha de escopos pelo nome
getDeclaracaoPilha :: String -> [Escopo] -> Either ErroEstado Declaracao
getDeclaracaoPilha nome pilha =
        case simbolo of
                Just simbolo' -> Right simbolo'
                Nothing -> Left $ ErroBuscaDeclaracao $ "Nome '" ++ nome ++ "' não encontrado na tabela de símbolos"
        where simbolo = getDeclaracaoPilha' nome (Just (head pilha)) pilha

-- Auxiliar para a busca por um símbolo na pilha
getDeclaracaoPilha' :: String -> Maybe Escopo -> [Escopo] -> Maybe Declaracao
getDeclaracaoPilha' _ Nothing _ = Nothing
getDeclaracaoPilha' nome (Just (_, idEscopoAnterior, tabelaDeclaracaos)) pilha =
        case simbolo of
                Just simbolo' -> Just simbolo'
                Nothing -> getDeclaracaoPilha' nome (getEscopoByIdFromPilha idEscopoAnterior pilha) pilha
        where simbolo = getDeclaracaoTabela nome tabelaDeclaracaos

-- Atualiza um símbolo no estado passado
atualizarDeclaracao :: Declaracao -> Estado -> Either ErroEstado Estado
atualizarDeclaracao simbolo (pilhaAtual, tipos, funcoes) =
        case pilhaAtualizada of
                Right pilhaAtualizada -> Right $ (pilhaAtualizada, tipos, funcoes)
                Left error -> Left error
        where pilhaAtualizada = atualizarDeclaracaoPilha simbolo pilhaAtual

-- Atualiza um símbolo no pilha de escopos passada
atualizarDeclaracaoPilha :: Declaracao -> [Escopo] -> Either ErroEstado [Escopo]
atualizarDeclaracaoPilha simbolo@(nome, _, _) pilhaAtual = 
        case pilha of
                Just pilhaAtualizada -> Right pilhaAtualizada
                Nothing -> Left $ ErroBuscaDeclaracao $ "Nome '" ++ nome ++ "' não encontrado na tabela de símbolos"
        where pilha = atualizarDeclaracaoPilha' simbolo (Just (head pilhaAtual)) pilhaAtual

-- Auxiliar para a atualização do símbolo na pilha
atualizarDeclaracaoPilha' :: Declaracao -> Maybe Escopo -> [Escopo] -> Maybe [Escopo]
atualizarDeclaracaoPilha' _ Nothing _ = Nothing
atualizarDeclaracaoPilha' simbolo (Just escopoAtual@(idEscopoAtual, idEscopoAnterior, tabelaAtual)) pilha =
        case tabela of
                Just tabelaAtualizada -> Just $ inicio ++ [(idEscopoAtual, idEscopoAnterior, tabelaAtualizada)] ++ tail fim
                Nothing -> atualizarDeclaracaoPilha' simbolo (getEscopoByIdFromPilha idEscopoAnterior pilha) pilha
        where tabela = atualizarDeclaracaoTabela simbolo tabelaAtual
              (inicio, fim) = genericSplitAt idEscopoAtual pilha

-- Atualiza um símbolo na tabela de símbolos
atualizarDeclaracaoTabela :: Declaracao -> [Declaracao] -> Maybe [Declaracao]
atualizarDeclaracaoTabela _ [] = Nothing
atualizarDeclaracaoTabela simbolo@(nome, _, _) (simboloAtual@(nome', _, _):tabelaAtual) =
        if nome == nome' then
                Just $ simbolo:tabelaAtual
        else
                case tabela of
                        Just tabelaAtualizada -> Just $ simboloAtual:tabelaAtualizada
                        Nothing -> Nothing
                where tabela = atualizarDeclaracaoTabela simbolo tabelaAtual
