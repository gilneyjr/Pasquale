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

criarEscopo :: Integer -> Estado -> Estado
criarEscopo idEscopoAtual (pilhaEscopo, tipos) =
            (((toInteger (length pilhaEscopo)) + 1, idEscopoAtual, []):pilhaEscopo, tipos)

getEscopoById :: Integer -> Estado -> Escopo
getEscopoById idEscopoAtual (pilhaEscopo, _) = getEscopo' idEscopoAtual pilhaEscopo

getEscopo' :: Integer -> [Escopo] -> Escopo
getEscopo' _             []     = error "Pilha de escopos vazia"
getEscopo' idEscopoAtual (escopo@(idEscopo, _, _):pilhaEscopo) =
           if idEscopoAtual == idEscopo then
               escopo
           else
               getEscopo' idEscopoAtual pilhaEscopo

getEscopoAtual :: Estado -> Escopo
getEscopoAtual ((escopoAtual:_), _) = escopoAtual

getIdEscopoAtual :: Estado -> Integer
getIdEscopoAtual estado = idEscopoAtual where (idEscopoAtual, _, _) = getEscopoAtual estado

removerEscopo :: Estado -> Estado
removerEscopo ((escopo:pilhaEscopo), tipos) = (pilhaEscopo, tipos)


