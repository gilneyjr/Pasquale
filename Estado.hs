module Estado (
    Estado,
    Escopo,
    Simbolo,
    Tipo,
    Valor,
    criarEscopo,
    getEscopoById,
    getEscopoAtual,
    getIdEscopoAtual,
    removerEscopo,
    addSimbolo,
    getSimbolo
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
            ((toInteger (length pilhaEscopo) + 1, idEscopoAtual, []):pilhaEscopo, tipos)

{- Parâmetros:
	Integer -> Id do Escopo a ser procurado
	Estado  -> Estado atual do programa
-}
getEscopoById :: Integer -> Estado -> Escopo
getEscopoById idEscopoAtual estado = getEscopoByIdFromEscopos idEscopoAtual (fst estado)

-- Função auxiliar para getEscopoById
getEscopoByIdFromEscopos :: Integer -> [Escopo] -> Escopo
getEscopoByIdFromEscopos _             []     = error "Pilha de escopos vazia"
getEscopoByIdFromEscopos idEscopoAtual (escopo@(idEscopo, _, _):pilhaEscopo) =
        if idEscopoAtual == idEscopo then
                escopo
        else
                getEscopoByIdFromEscopos idEscopoAtual pilhaEscopo

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
        if isRight escopoAtualizado then
                Right ((head $ rights $ [escopoAtualizado]):tail escopos, tipos)
        else
                Left $ head $ lefts [escopoAtualizado]
        where escopoAtualizado = addSimboloEscopo simbolo (getEscopoAtual estado)

-- Adicionar símbolo ao escopo atual
addSimboloEscopo :: Simbolo -> Escopo -> Either ErroEstado Escopo
addSimboloEscopo simbolo escopo@(idEscopo, idEscopoAnterior, tabela) = 
        if isRight tabelaAtualizada then
                Right (idEscopo, idEscopoAnterior, head $ rights $ [tabelaAtualizada])
        else
                Left $ head $ lefts [tabelaAtualizada]
        where tabelaAtualizada = addSimboloTabela simbolo tabela

-- Adicionar simbolo à tabela de símbolos
addSimboloTabela :: Simbolo -> [Simbolo] -> Either ErroEstado [Simbolo] 
addSimboloTabela simbolo@(nome, _, _) tabela =
        if getSimboloTabela nome tabela == Nothing then
                Right $ simbolo:tabela
        else
                Left $ ErroNomeDuplicado ("Já existe uma variável com o nome:" ++ nome)

-- Busca por um símbolo na tabela de símbolos pelo nome
getSimboloTabela :: String -> [Simbolo] -> Maybe Simbolo
getSimboloTabela _    [] = Nothing
getSimboloTabela nomeNovo (simbolo@(nome, _, _):tabela) =
        if nomeNovo == nome then
                Just simbolo
        else
                getSimboloTabela nomeNovo tabela

-- Busca por um símbolo no estado pelo nome
getSimbolo :: String -> Estado -> Simbolo
getSimbolo nome estado = getSimboloPilha nome (fst estado)

-- Busca por um síbolo na pilha de escopos pelo nome
getSimboloPilha :: String -> [Escopo] -> Simbolo
getSimboloPilha nome pilha =
        if simbolo /= Nothing then
                fromJust simbolo
        else
                error $ "Nome '" ++ nome ++ "' não encontrado na tabela de símbolos"
        where simbolo = getSimboloPilha' nome (head pilha) pilha

-- Auxiliar para a busca por um símbolo na pilha
getSimboloPilha' :: String -> Escopo -> [Escopo] -> Maybe Simbolo
getSimboloPilha' nome escopo@(_, idEscopoAnterior, _) pilha =
        if simbolo /= Nothing then
                simbolo
        else
                getSimboloPilha' nome (getEscopoByIdFromEscopos idEscopoAnterior pilha) pilha
        where simbolo = getSimboloEscopo nome escopo

-- Buscar por um símbolo no escopo pelo nome
getSimboloEscopo :: String -> Escopo -> Maybe Simbolo
getSimboloEscopo nome (_, _, tabelaSimbolos) = getSimboloTabela nome tabelaSimbolos

atualizarSimbolo :: Simbolo -> Estado -> Estado
atualizarSimbolo = undefined
--atualizarSimbolo simbolo estado = atualizarSimboloPilha simbolo (fst estado)
