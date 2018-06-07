module Estado (
    Estado,
    Escopo,
    Simbolo,
    Tipo (..),
    Valor (..),
    ErroEstado (..),
    criarEscopo,
    getEscopoById,
    getEscopoAtual,
    getIdEscopoAtual,
    removerEscopo,
    addSimbolo,
    getSimbolo,
    atualizarSimbolo
) where

import Arvore
import Data.Maybe
import Data.Either

-- pilha de escopos, lista de tipos
type Estado = ([Escopo], [Tipo])

-- Número do Escopo, Número do escopo anterior, Tabela de símbolos
type Escopo = (Integer, Integer, [Simbolo])

-- Nome, Tipo, Valor
type Simbolo = (String, Tipo, Valor)

data Tipo = TipoAtomico String
          | TipoVetor Tipo
          | TipoPonteiro Tipo
          | TipoEstrutura String
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
                | ErroBuscaSimbolo String
                deriving (Show)

estadoNulo :: Estado
estadoNulo = ([], [])

escopoNulo :: Escopo
escopoNulo = (-1, -1, [])

{- Parâmetros:
	Integer -> Id do Escopo pai
	Estado  -> Estado atual do programa
-}
criarEscopo :: Integer -> Estado -> Estado
criarEscopo idEscopoAtual (pilhaEscopo, tipos) =
            ((toInteger ((length pilhaEscopo) + 1), idEscopoAtual, []):pilhaEscopo, tipos)

{- Parâmetros:
	Integer -> Id do Escopo a ser procurado
	Estado  -> Estado atual do programa
-}
getEscopoById :: Integer -> Estado -> Escopo
getEscopoById idEscopoAtual estado = fromJust $ getEscopoByIdFromPilha idEscopoAtual (fst estado)

-- Função auxiliar para getEscopoById
getEscopoByIdFromPilha :: Integer -> [Escopo] -> Maybe Escopo
getEscopoByIdFromPilha _        []    = Nothing
getEscopoByIdFromPilha idEscopo pilha = Just $ pilha !! fromInteger idEscopo

-- Retorna o escopo atual a partir do estado
getEscopoAtual :: Estado -> Escopo
getEscopoAtual = head . fst

-- Retorna o Id do escopo atual
getIdEscopoAtual :: Estado -> Integer
getIdEscopoAtual estado = idEscopoAtual where (idEscopoAtual, _, _) = getEscopoAtual estado

-- Remove o escopo do topo da pilha
removerEscopo :: Estado -> Estado
removerEscopo ((escopo:pilhaEscopo), tipos) = (pilhaEscopo, tipos)

-- Adicionar símbolo ao estado
addSimbolo :: Simbolo -> Estado -> Either ErroEstado Estado
addSimbolo simbolo estado@(escopos, tipos) = 
        case escopo of
                Right escopoAtualizado -> Right $ (escopoAtualizado: tail escopos, tipos)
                Left erro -> Left erro
        where escopo = addSimboloEscopo simbolo (getEscopoAtual estado)

-- Adicionar símbolo ao escopo atual
addSimboloEscopo :: Simbolo -> Escopo -> Either ErroEstado Escopo
addSimboloEscopo simbolo (idEscopo, idEscopoAnterior, tabelaAtual) = 
        case tabela of 
                Right tabelaAtualizada -> Right (idEscopo, idEscopoAnterior, tabelaAtualizada)
                Left erro -> Left erro
        where tabela = addSimboloTabela simbolo tabelaAtual

-- Adicionar simbolo à tabela de símbolos
addSimboloTabela :: Simbolo -> [Simbolo] -> Either ErroEstado [Simbolo] 
addSimboloTabela (nome, _, _) tabela =
        case getSimboloTabela nome tabela of
                Just simbolo -> Right $ simbolo:tabela
                Nothing -> Left $ ErroNomeDuplicado ("Já existe uma variável com o nome:" ++ nome)

-- Busca por um símbolo na tabela de símbolos pelo nome
getSimboloTabela :: String -> [Simbolo] -> Maybe Simbolo
getSimboloTabela _    [] = Nothing
getSimboloTabela nomeNovo (simbolo@(nome, _, _):tabela) =
        if nomeNovo == nome then
                Just simbolo
        else
                getSimboloTabela nomeNovo tabela

-- Busca por um símbolo no estado pelo nome
getSimbolo :: String -> Estado -> Either ErroEstado Simbolo
getSimbolo nome estado = getSimboloPilha nome (fst estado)

-- Busca por um síbolo na pilha de escopos pelo nome
getSimboloPilha :: String -> [Escopo] -> Either ErroEstado Simbolo
getSimboloPilha nome pilha =
        case simbolo of
                Just simbolo' -> Right simbolo'
                Nothing -> Left $ ErroBuscaSimbolo $ "Nome '" ++ nome ++ "' não encontrado na tabela de símbolos"
        where simbolo = getSimboloPilha' nome (Just (head pilha)) pilha

-- Auxiliar para a busca por um símbolo na pilha
getSimboloPilha' :: String -> Maybe Escopo -> [Escopo] -> Maybe Simbolo
getSimboloPilha' _ Nothing _ = Nothing
getSimboloPilha' nome (Just (_, idEscopoAnterior, tabelaSimbolos)) pilha =
        case simbolo of
                Just simbolo' -> Just simbolo'
                Nothing -> getSimboloPilha' nome (getEscopoByIdFromPilha idEscopoAnterior pilha) pilha
        where simbolo = getSimboloTabela nome tabelaSimbolos

-- Buscar por um símbolo no escopo pelo nome
getSimboloEscopo :: String -> Escopo -> Maybe Simbolo
getSimboloEscopo nome (_, _, tabelaSimbolos) = getSimboloTabela nome tabelaSimbolos

atualizarSimbolo :: Simbolo -> Estado -> Either ErroEstado Estado
atualizarSimbolo simbolo (pilhaAtual, tipos) =
        case pilhaAtualizada of
                Right pilhaAtualizada -> Right $ (pilhaAtualizada, tipos)
                Left error -> Left error
        where pilhaAtualizada = atualizarSimboloPilha simbolo pilhaAtual

atualizarSimboloPilha :: Simbolo -> [Escopo] -> Either ErroEstado [Escopo]
atualizarSimboloPilha simbolo@(nome, _, _) pilhaAtual = 
        case pilha of
                Just pilhaAtualizada -> Right pilhaAtualizada
                Nothing -> Left $ ErroBuscaSimbolo $ "Nome '" ++ nome ++ "' não encontrado na tabela de símbolos"
        where pilha = atualizarSimboloPilha' simbolo (Just (head pilhaAtual)) pilhaAtual

atualizarSimboloPilha' :: Simbolo -> Maybe Escopo -> [Escopo] -> Maybe [Escopo]
atualizarSimboloPilha' _ Nothing _ = Nothing
atualizarSimboloPilha' simbolo (Just escopoAtual@(idEscopoAtual, idEscopoAnterior, tabelaAtual)) pilha =
        case tabela of
                Just tabelaAtualizada -> Just $ trocarEscopo idEscopoAtual (idEscopoAtual, idEscopoAnterior, tabelaAtualizada) pilha
                Nothing -> atualizarSimboloPilha' simbolo (getEscopoByIdFromPilha idEscopoAnterior pilha) pilha
        where tabela = undefined

trocarEscopo :: Integer -> Escopo -> [Escopo] -> [Escopo]
trocarEscopo i escopo pilha = inicio ++ [escopo] ++ tail fim
        where (inicio, fim) = splitAt (fromInteger i) pilha
