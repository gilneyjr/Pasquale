module Estado (
    Estado,
    Escopo,
    Subprograma,
    Funcao,
    Procedimento,
    Assinatura,
    ErroEstado (..),
    criarEscopo,
    getEscopoById,
    getEscopoAtual,
    getIdEscopoAtual,
    removerEscopo,
    addVariavel,
    getVariavel,
    removerVariavel,
    atualizarVariavel,
    addTipo,
    addSubprograma,
    getSubprograma,
    getTipo
) where

import Tipos
import Arvore
import Data.Maybe
import Data.Either
import Data.List
--import Debug.Trace

-- pilha de escopos, lista de tipos
type Estado = ([Escopo], [Tipo], [Subprograma])

-- Número do Escopo, Número do escopo anterior, Tabela de declaracoes
type Escopo = (Integer, Integer, [Variavel])

type Subprograma = Either Procedimento Funcao

type Funcao = (String, [Declaracao], [STMT], Tipo)

type Procedimento = (String, [Declaracao], [STMT])

type Assinatura = (String, [Declaracao])

data ErroEstado = ErroNomeDuplicado String
                | ErroTipoDuplicado String
                | ErroSubprogramaDuplicada String
                | ErroBuscaTipo String
                | ErroBuscaVariavel String
                | ErroSubprogramaNaoEncontrado String

instance Show ErroEstado where
    show (ErroNomeDuplicado s)            = s
    show (ErroTipoDuplicado s)            = s
    show (ErroSubprogramaDuplicada s)     = s
    show (ErroBuscaTipo s)                = s
    show (ErroBuscaVariavel s)            = s
    show (ErroSubprogramaNaoEncontrado s) = s

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
getEscopoByIdFromPilha 0        _     = Nothing
getEscopoByIdFromPilha idEscopo pilha = Just $ genericIndex pilha ((genericLength pilha) - idEscopo)

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
addVariavelTabela variavel@(nome, _, _) tabela =
    case getVariavelTabela nome tabela of
        Just _ -> Left $ ErroNomeDuplicado ("Já existe uma variável com o nome: " ++ nome)
        Nothing -> Right $ variavel:tabela

-- Busca por um símbolo na tabela de símbolos pelo nome
getVariavelTabela :: String -> [Variavel] -> Maybe Variavel
getVariavelTabela nome tabela = find (\(nome', _, _) -> nome == nome') tabela

-- Busca por um símbolo no estado pelo nome
getVariavel :: String -> Estado -> Either ErroEstado Variavel
getVariavel nome (pilhaEscopo, _, _) = getVariavelPilha nome pilhaEscopo

-- Busca por um síbolo na pilha de escopos pelo nome
getVariavelPilha :: String -> [Escopo] -> Either ErroEstado Variavel
getVariavelPilha nome pilha =
    case simbolo of
        Just simbolo' -> Right simbolo'
        Nothing -> Left $ ErroBuscaVariavel $ " '" ++ nome ++ "' não encontrado!"
    where simbolo = getVariavelPilha' nome (Just (head pilha)) pilha

-- Auxiliar para a busca por um símbolo na pilha
getVariavelPilha' :: String -> Maybe Escopo -> [Escopo] -> Maybe Variavel
getVariavelPilha' _ Nothing _ = Nothing
getVariavelPilha' nome (Just (_, idEscopoAnterior, tabelaVariavels)) pilha =
    case simbolo of
        Just simbolo' -> Just simbolo'
        Nothing -> do
            getVariavelPilha' nome (getEscopoByIdFromPilha idEscopoAnterior pilha) pilha
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
        Nothing -> Left $ ErroBuscaVariavel $ "'" ++ nome ++ "' não encontrado!"
    where pilha = atualizarVariavelPilha' simbolo (Just (head pilhaAtual)) pilhaAtual

-- Auxiliar para a atualização do símbolo na pilha
atualizarVariavelPilha' :: Variavel -> Maybe Escopo -> [Escopo] -> Maybe [Escopo]
atualizarVariavelPilha' _ Nothing _ = Nothing
atualizarVariavelPilha' simbolo (Just escopoAtual@(idEscopoAtual, idEscopoAnterior, tabelaAtual)) pilha =
    case tabela of
        Just tabelaAtualizada -> Just $ inicio ++ [(idEscopoAtual, idEscopoAnterior, tabelaAtualizada)] ++ (tail fim)
        Nothing -> atualizarVariavelPilha' simbolo (getEscopoByIdFromPilha idEscopoAnterior pilha) pilha
    where tabela = atualizarVariavelTabela simbolo tabelaAtual
          (inicio, fim) = genericSplitAt ((genericLength pilha) - idEscopoAtual) pilha

-- Atualiza um símbolo na tabela de símbolos
atualizarVariavelTabela :: Variavel -> [Variavel] -> Maybe [Variavel]
atualizarVariavelTabela simbolo@(nome, _, _) tabela =
    case index of
        Just index' -> 
            let (inicio, fim) = genericSplitAt index' tabela in
            Just $ inicio ++ [simbolo] ++ (tail fim)
        Nothing -> Nothing
    where index = findIndex (\(nome', _, _) -> nome == nome') tabela        

-- Remove um símbolo no estado passado
removerVariavel :: Variavel -> Estado -> Either ErroEstado Estado
removerVariavel simbolo (pilhaAtual, tipos, funcoes) =
    case pilhaAtualizada of
        Right pilhaAtualizada -> Right $ (pilhaAtualizada, tipos, funcoes)
        Left error -> Left error
    where pilhaAtualizada = removerVariavelPilha simbolo pilhaAtual

-- Remove um símbolo no pilha de escopos passada
removerVariavelPilha :: Variavel -> [Escopo] -> Either ErroEstado [Escopo]
removerVariavelPilha simbolo@(nome, _, _) pilhaAtual = 
    case pilha of
        Just pilhaAtualizada -> Right pilhaAtualizada
        Nothing -> Left $ ErroBuscaVariavel $ "Nome '" ++ nome ++ "' não encontrado na tabela de símbolos"
    where pilha = removerVariavelPilha' simbolo (Just (head pilhaAtual)) pilhaAtual

-- Auxiliar para a remoção do símbolo na pilha
removerVariavelPilha' :: Variavel -> Maybe Escopo -> [Escopo] -> Maybe [Escopo]
removerVariavelPilha' _ Nothing _ = Nothing
removerVariavelPilha' simbolo (Just escopoAtual@(idEscopoAtual, idEscopoAnterior, tabelaAtual)) pilha =
    case tabela of
        Just tabelaAtualizada -> Just $ inicio ++ [(idEscopoAtual, idEscopoAnterior, tabelaAtualizada)] ++ (tail fim)
        Nothing -> removerVariavelPilha' simbolo (getEscopoByIdFromPilha idEscopoAnterior pilha) pilha
    where tabela = removerVariavelTabela simbolo tabelaAtual
          (inicio, fim) = genericSplitAt ((genericLength pilha) - idEscopoAtual) pilha

-- Remove um símbolo na tabela de símbolos
removerVariavelTabela :: Variavel -> [Variavel] -> Maybe [Variavel]
removerVariavelTabela simbolo@(nome, _, _) tabela =
    case index of
        Just index' -> 
            let (inicio, fim) = genericSplitAt index' tabela in
            Just $ inicio ++ (tail fim)
        Nothing -> Nothing
    where index = findIndex (\(nome', _, _) -> nome == nome') tabela     

-- Adiciona um tipo no estado
addTipo :: Tipo -> Estado -> Either ErroEstado Estado
addTipo tipo (pilha, tiposAtuais, funcoes) =
    case tipos of
        Right tiposAtualizados -> Right $ (pilha, tiposAtualizados, funcoes)
        Left error -> Left error
    where tipos = addTipoLista tipo tiposAtuais

-- Adiciona um tipo na lista de tipos
addTipoLista :: Tipo -> [Tipo] -> Either ErroEstado [Tipo]
addTipoLista tipo tipos =
    if notElem tipo tipos then
        Right $ tipo:tipos
    else
        Left $ ErroTipoDuplicado $ "Tipo '" ++ show tipo ++ "' já foi declarado anteriormente"

-- Busca um tipo primitivo no estado
getTipo :: String -> Estado -> Either ErroEstado Tipo
getTipo nome (pilha, tipos, funcoes) =
    case tipo of
        Just tipoEncontrado -> Right tipoEncontrado
        Nothing -> Left $ ErroBuscaTipo $ "Tipo '" ++ nome ++ "' não encontrado"
    where tipo = getTipoLista nome tipos

getTipoLista :: String -> [Tipo] -> Maybe Tipo
getTipoLista _ [] = Nothing
getTipoLista nome (tipo:tipos) =
    case tipo of
        TipoAtomico nome' -> if (nome == nome') then Just tipo else getTipoLista nome tipos
        TipoEstrutura nome' _ -> if (nome == nome') then Just tipo else getTipoLista nome tipos
        otherwise -> getTipoLista nome tipos

getTipoAtomico :: String -> [Tipo] -> Maybe Tipo
getTipoAtomico nome = find (\x -> (TipoAtomico nome) == x)

getTipoEstrutura :: String -> [Tipo] -> Maybe Tipo
getTipoEstrutura _    [] = Nothing
getTipoEstrutura nome (tipo@(TipoEstrutura nome' _):tipos) = 
    if nome == nome' then
        Just tipo
    else
        getTipoEstrutura nome tipos
getTipoEstrutura nome (_:tipos) = getTipoEstrutura nome tipos

-- Adiciona um subprograma no estado
addSubprograma :: Subprograma -> Estado -> Either ErroEstado Estado
addSubprograma subprograma (pilha, tipos, subprogramasAtuais) =
    case subprogramas of
        Right subprogramasAtualizadas -> Right (pilha, tipos, subprogramasAtualizadas)
        Left error -> Left error
    where subprogramas = addSubprogramaLista subprograma subprogramasAtuais

-- Adiciona um subprograma na lista de subprogramas
addSubprogramaLista :: Subprograma -> [Subprograma] -> Either ErroEstado [Subprograma]
addSubprogramaLista subprograma subprogramas = 
    case getSubprogramaLista assinatura subprogramas of
        Just _ -> Left $ ErroSubprogramaDuplicada $ "Função '" ++ nome ++ "' já foi criada com a mesma assinatura"
        Nothing -> Right $ subprograma:subprogramas
    where assinatura@(nome, _) = getAssinaturaSubprograma subprograma

-- Retorna a assinatura de um subprograma
getAssinaturaSubprograma :: Subprograma -> Assinatura
getAssinaturaSubprograma (Left (nome, parametros, _)) = (nome, parametros)
getAssinaturaSubprograma (Right (nome, parametros, _, _)) = (nome, parametros)

-- Busca por um subprograma no estado atraves de sua assinatura
getSubprograma :: Assinatura -> Estado -> Either ErroEstado Subprograma
getSubprograma assinatura@(nome, _) (_, _, funcoes) = 
    case getSubprogramaLista assinatura funcoes of
        Just subprograma -> Right subprograma
        Nothing -> Left $ ErroSubprogramaNaoEncontrado $ "Subprograma '" ++ nome ++ "' não encontrado"

-- Busca por um subprograma na lista de subprogrmas atraves de sua assinatura
getSubprogramaLista :: Assinatura -> [Subprograma] -> Maybe Subprograma
getSubprogramaLista assinatura subprogramas = find (\x -> getAssinaturaSubprograma x == assinatura) subprogramas
