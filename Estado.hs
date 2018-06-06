module Estado where

import Arvore

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

{- Parâmetros:
	Integer -> Id do Escopo pai
	Estado  -> Estado atual do programa
-}
criarEscopo :: Integer -> Estado -> Estado
criarEscopo idEscopoAtual (pilhaEscopo, tipos) =
            (((toInteger (length pilhaEscopo)) + 1, idEscopoAtual, []):pilhaEscopo, tipos)

{- Parâmetros:
	Integer -> Id do Escopo a ser procurado
	Estado  -> Estado atual do programa
-}
getEscopoById :: Integer -> Estado -> Escopo
getEscopoById idEscopoAtual estado = getEscopo' idEscopoAtual (fst estado)

-- Função auxiliar para getEscopoById
getEscopo' :: Integer -> [Escopo] -> Escopo
getEscopo' _             []     = error "Pilha de escopos vazia"
getEscopo' idEscopoAtual (escopo@(idEscopo, _, _):pilhaEscopo) =
           if idEscopoAtual == idEscopo then
               escopo
           else
               getEscopo' idEscopoAtual pilhaEscopo

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
addSimbolo :: Simbolo -> Estado -> Estado
addSimbolo simbolo estado@(escopos, tipos) = ((addSimboloEscopo simbolo (getEscopoAtual estado)):tail escopos, tipos)

-- Adicionar símbolo ao escopo atual
addSimboloEscopo :: Simbolo -> Escopo -> Escopo
addSimboloEscopo simbolo escopo@(idEscopo, idEscopoAnterior, tabela) = (idEscopo, idEscopoAnterior, addSimboloTabela simbolo tabela)

-- Adicionar simbolo à tabela de símbolos
addSimboloTabela :: Simbolo -> [Simbolo] -> [Simbolo] 
addSimboloTabela simbolo@(nome, _, _) tabela =
        if getSimbolo nome tabela == Nothing then
                simbolo:tabela
        else
                error "Símbolo já existente com esse nome"

-- Busca por um símbolo na tabela de símbolos pelo nome
getSimbolo :: String -> [Simbolo] -> Maybe Simbolo
getSimbolo _    [] = Nothing
getSimbolo nomeNovo (simbolo@(nome, _, _):tabela) =
        if nomeNovo == nome then
                Just simbolo
        else
                getSimbolo nomeNovo tabela
