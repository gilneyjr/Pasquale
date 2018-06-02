-- Types
-- Version: 14/06/2017
module Types where

-- External imports
import Data.List
-- Internal imports
import Lexico

-- -----------------------------------------------------------------------------
-- Values
-- -----------------------------------------------------------------------------

-- - Token null
tokenNull :: Token
tokenNull = (ESTRUTURA (-1,-1))

-- - Get default value of different types
-- Type   Variable type
-- Return Initial variable value
getTokenPosition :: Token -> String
getTokenPosition (ESTRUTURA pos) = show pos
getTokenPosition _ = error "Invalid token format."
