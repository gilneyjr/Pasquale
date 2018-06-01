module ParserTokens where
import Data.Functor.Identity
import Lexico
import Text.Parsec

estruturatoken :: ParsecT [Token] u Identity Token
estruturatoken = tokenPrim show update_pos get_token where
    get_token ( ESTRUTURA x ) = Just ( ESTRUTURA x )
    get_token _ = Nothing

fimestruturatoken :: ParsecT [Token] u Identity Token
fimestruturatoken = tokenPrim show update_pos get_token where
    get_token ( FIMESTRUTURA x ) = Just ( FIMESTRUTURA x )
    get_token _ = Nothing

funcaotoken :: ParsecT [Token] u Identity Token
funcaotoken = tokenPrim show update_pos get_token where
    get_token ( FUNCAO x ) = Just ( FUNCAO x )
    get_token _ = Nothing

fimfuncaotoken :: ParsecT [Token] u Identity Token
fimfuncaotoken = tokenPrim show update_pos get_token where
    get_token ( FIMFUNCAO x ) = Just ( FIMFUNCAO x )
    get_token _ = Nothing

procedimentotoken :: ParsecT [Token] u Identity Token
procedimentotoken = tokenPrim show update_pos get_token where
    get_token ( PROCEDIMENTO x ) = Just ( PROCEDIMENTO x )
    get_token _ = Nothing

fimprocedimentotoken :: ParsecT [Token] u Identity Token
fimprocedimentotoken = tokenPrim show update_pos get_token where
    get_token ( FIMPROCEDIMENTO x ) = Just ( FIMPROCEDIMENTO x )
    get_token _ = Nothing

operadortoken :: ParsecT [Token] u Identity Token
operadortoken = tokenPrim show update_pos get_token where
    get_token ( OPERADOR x ) = Just ( OPERADOR x )
    get_token _ = Nothing

fimoperadortoken :: ParsecT [Token] u Identity Token
fimoperadortoken = tokenPrim show update_pos get_token where
    get_token ( FIMOPERADOR x ) = Just ( FIMOPERADOR x )
    get_token _ = Nothing

recebetoken :: ParsecT [Token] u Identity Token
recebetoken = tokenPrim show update_pos get_token where
    get_token ( RECEBE x ) = Just ( RECEBE x )
    get_token _ = Nothing

retornatoken :: ParsecT [Token] u Identity Token
retornatoken = tokenPrim show update_pos get_token where
    get_token ( RETORNA x ) = Just ( RETORNA x )
    get_token _ = Nothing

retornetoken :: ParsecT [Token] u Identity Token
retornetoken = tokenPrim show update_pos get_token where
    get_token ( RETORNE x ) = Just ( RETORNE x )
    get_token _ = Nothing

principaltoken :: ParsecT [Token] u Identity Token
principaltoken = tokenPrim show update_pos get_token where
    get_token ( PRINCIPAL x ) = Just ( PRINCIPAL x )
    get_token _ = Nothing

fimprincipaltoken :: ParsecT [Token] u Identity Token
fimprincipaltoken = tokenPrim show update_pos get_token where
    get_token ( FIMPRINCIPAL x ) = Just ( FIMPRINCIPAL x )
    get_token _ = Nothing

saiatoken :: ParsecT [Token] u Identity Token
saiatoken = tokenPrim show update_pos get_token where
    get_token ( SAIA x ) = Just ( SAIA x )
    get_token _ = Nothing

continuetoken :: ParsecT [Token] u Identity Token
continuetoken = tokenPrim show update_pos get_token where
    get_token ( CONTINUE x ) = Just ( CONTINUE x )
    get_token _ = Nothing

setoken :: ParsecT [Token] u Identity Token
setoken = tokenPrim show update_pos get_token where
    get_token ( SE x ) = Just ( SE x )
    get_token _ = Nothing

entaotoken :: ParsecT [Token] u Identity Token
entaotoken = tokenPrim show update_pos get_token where
    get_token ( ENTAO x ) = Just ( ENTAO x )
    get_token _ = Nothing

senaotoken :: ParsecT [Token] u Identity Token
senaotoken = tokenPrim show update_pos get_token where
    get_token ( SENAO x ) = Just ( SENAO x )
    get_token _ = Nothing

fimsetoken :: ParsecT [Token] u Identity Token
fimsetoken = tokenPrim show update_pos get_token where
    get_token ( FIMSE x ) = Just ( FIMSE x )
    get_token _ = Nothing

enquantotoken :: ParsecT [Token] u Identity Token
enquantotoken = tokenPrim show update_pos get_token where
    get_token ( ENQUANTO x ) = Just ( ENQUANTO x )
    get_token _ = Nothing

executetoken :: ParsecT [Token] u Identity Token
executetoken = tokenPrim show update_pos get_token where
    get_token ( EXECUTE x ) = Just ( EXECUTE x )
    get_token _ = Nothing

fimenquantotoken :: ParsecT [Token] u Identity Token
fimenquantotoken = tokenPrim show update_pos get_token where
    get_token ( FIMENQUANTO x ) = Just ( FIMENQUANTO x )
    get_token _ = Nothing

definatoken :: ParsecT [Token] u Identity Token
definatoken = tokenPrim show update_pos get_token where
    get_token ( DEFINA x ) = Just ( DEFINA x )
    get_token _ = Nothing

slowoutoken :: ParsecT [Token] u Identity Token
slowoutoken = tokenPrim show update_pos get_token where
    get_token ( SlowOU x ) = Just ( SlowOU x )
    get_token _ = Nothing

slowetoken :: ParsecT [Token] u Identity Token
slowetoken = tokenPrim show update_pos get_token where
    get_token ( SlowE x ) = Just ( SlowE x )
    get_token _ = Nothing

outoken :: ParsecT [Token] u Identity Token
outoken = tokenPrim show update_pos get_token where
    get_token ( OU x ) = Just ( OU x )
    get_token _ = Nothing

etoken :: ParsecT [Token] u Identity Token
etoken = tokenPrim show update_pos get_token where
    get_token ( E x ) = Just ( E x )
    get_token _ = Nothing

logicotoken :: ParsecT [Token] u Identity Token
logicotoken = tokenPrim show update_pos get_token where
    get_token ( LOGICO x y ) = Just ( LOGICO x y )
    get_token _ = Nothing

ponttoken :: ParsecT [Token] u Identity Token
ponttoken = tokenPrim show update_pos get_token where
    get_token ( PONT x ) = Just ( PONT x )
    get_token _ = Nothing

novotoken :: ParsecT [Token] u Identity Token
novotoken = tokenPrim show update_pos get_token where
    get_token ( NOVO x ) = Just ( NOVO x )
    get_token _ = Nothing

deletetoken :: ParsecT [Token] u Identity Token
deletetoken = tokenPrim show update_pos get_token where
    get_token ( DELETE x ) = Just ( DELETE x )
    get_token _ = Nothing

consttoken :: ParsecT [Token] u Identity Token
consttoken = tokenPrim show update_pos get_token where
    get_token ( CONST x ) = Just ( CONST x )
    get_token _ = Nothing

leiatoken :: ParsecT [Token] u Identity Token
leiatoken = tokenPrim show update_pos get_token where
    get_token ( LEIA x ) = Just ( LEIA x )
    get_token _ = Nothing

escrevatoken :: ParsecT [Token] u Identity Token
escrevatoken = tokenPrim show update_pos get_token where
    get_token ( ESCREVA x ) = Just ( ESCREVA x )
    get_token _ = Nothing

referenciatoken :: ParsecT [Token] u Identity Token
referenciatoken = tokenPrim show update_pos get_token where
    get_token ( REFERENCIA x ) = Just ( REFERENCIA x )
    get_token _ = Nothing

valortoken :: ParsecT [Token] u Identity Token
valortoken = tokenPrim show update_pos get_token where
    get_token ( VALOR x ) = Just ( VALOR x )
    get_token _ = Nothing

attribtoken :: ParsecT [Token] u Identity Token
attribtoken = tokenPrim show update_pos get_token where
    get_token ( Attrib x ) = Just ( Attrib x )
    get_token _ = Nothing

geqtoken :: ParsecT [Token] u Identity Token
geqtoken = tokenPrim show update_pos get_token where
    get_token ( Geq x ) = Just ( Geq x )
    get_token _ = Nothing

leqtoken :: ParsecT [Token] u Identity Token
leqtoken = tokenPrim show update_pos get_token where
    get_token ( Leq x ) = Just ( Leq x )
    get_token _ = Nothing

difftoken :: ParsecT [Token] u Identity Token
difftoken = tokenPrim show update_pos get_token where
    get_token ( Diff x ) = Just ( Diff x )
    get_token _ = Nothing

equaltoken :: ParsecT [Token] u Identity Token
equaltoken = tokenPrim show update_pos get_token where
    get_token ( Equal x ) = Just ( Equal x )
    get_token _ = Nothing

greattoken :: ParsecT [Token] u Identity Token
greattoken = tokenPrim show update_pos get_token where
    get_token ( Great x ) = Just ( Great x )
    get_token _ = Nothing

lesstoken :: ParsecT [Token] u Identity Token
lesstoken = tokenPrim show update_pos get_token where
    get_token ( Less x ) = Just ( Less x )
    get_token _ = Nothing

addtoken :: ParsecT [Token] u Identity Token
addtoken = tokenPrim show update_pos get_token where
    get_token ( Add x ) = Just ( Add x )
    get_token _ = Nothing

subtoken :: ParsecT [Token] u Identity Token
subtoken = tokenPrim show update_pos get_token where
    get_token ( Sub x ) = Just ( Sub x )
    get_token _ = Nothing

multtoken :: ParsecT [Token] u Identity Token
multtoken = tokenPrim show update_pos get_token where
    get_token ( Mult x ) = Just ( Mult x )
    get_token _ = Nothing

divtoken :: ParsecT [Token] u Identity Token
divtoken = tokenPrim show update_pos get_token where
    get_token ( Div x ) = Just ( Div x )
    get_token _ = Nothing

modtoken :: ParsecT [Token] u Identity Token
modtoken = tokenPrim show update_pos get_token where
    get_token ( MOD x ) = Just ( MOD x )
    get_token _ = Nothing

nottoken :: ParsecT [Token] u Identity Token
nottoken = tokenPrim show update_pos get_token where
    get_token ( NOT x ) = Just ( NOT x )
    get_token _ = Nothing

openbracktoken :: ParsecT [Token] u Identity Token
openbracktoken = tokenPrim show update_pos get_token where
    get_token ( OpenBrack x ) = Just ( OpenBrack x )
    get_token _ = Nothing

closebracktoken :: ParsecT [Token] u Identity Token
closebracktoken = tokenPrim show update_pos get_token where
    get_token ( CloseBrack x ) = Just ( CloseBrack x )
    get_token _ = Nothing

opensqbracktoken :: ParsecT [Token] u Identity Token
opensqbracktoken = tokenPrim show update_pos get_token where
    get_token ( OpenSqBrack x ) = Just ( OpenSqBrack x )
    get_token _ = Nothing

closesqbracktoken :: ParsecT [Token] u Identity Token
closesqbracktoken = tokenPrim show update_pos get_token where
    get_token ( CloseSqBrack x ) = Just ( CloseSqBrack x )
    get_token _ = Nothing

commatoken :: ParsecT [Token] u Identity Token
commatoken = tokenPrim show update_pos get_token where
    get_token ( Comma x ) = Just ( Comma x )
    get_token _ = Nothing

dottoken :: ParsecT [Token] u Identity Token
dottoken = tokenPrim show update_pos get_token where
    get_token ( Dot x ) = Just ( Dot x )
    get_token _ = Nothing

endcommandtoken :: ParsecT [Token] u Identity Token
endcommandtoken = tokenPrim show update_pos get_token where
    get_token ( EndCommand x ) = Just ( EndCommand x )
    get_token _ = Nothing

realtoken :: ParsecT [Token] u Identity Token
realtoken = tokenPrim show update_pos get_token where
    get_token ( REAL x y ) = Just ( REAL x y )
    get_token _ = Nothing

inteirotoken :: ParsecT [Token] u Identity Token
inteirotoken = tokenPrim show update_pos get_token where
    get_token ( INTEIRO x y ) = Just ( INTEIRO x y )
    get_token _ = Nothing

caracteretoken :: ParsecT [Token] u Identity Token
caracteretoken = tokenPrim show update_pos get_token where
    get_token ( CARACTERE x y ) = Just ( CARACTERE x y )
    get_token _ = Nothing

textotoken :: ParsecT [Token] u Identity Token
textotoken = tokenPrim show update_pos get_token where
    get_token ( TEXTO x y ) = Just ( TEXTO x y )
    get_token _ = Nothing

tipotoken :: ParsecT [Token] u Identity Token
tipotoken = tokenPrim show update_pos get_token where
    get_token ( TIPO x y ) = Just ( TIPO x y )
    get_token _ = Nothing

idtoken :: ParsecT [Token] u Identity Token
idtoken = tokenPrim show update_pos get_token where
    get_token ( ID x y ) = Just ( ID x y )
    get_token _ = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos
update_pos pos _ []      = pos
