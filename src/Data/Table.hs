module Data.Table where

import Data.Schema
import Data.Parser


data Value
  = IValue Integer
  | BValue Bool
  | SValue String
  | FValue Double
  deriving (Eq, Ord, Show)

newtype Record
  = Record {unRecord :: [(Name,Value)]}
    deriving (Eq, Ord, Show)

data Table
  = Table {
      schema :: Schema
    , records :: [Record]
    } deriving (Eq, Ord, Show)

parseVal :: Type -> Parser Value
parseVal t
  | t == integer = IValue <$> intParser
  | t == float   = FValue <$> floatParser
  | t == bool    = BValue <$> boolParser
  | t == string  = SValue <$> stringParser
  | otherwise    = fail "Parse Error!"


parseValue :: String -> Type -> Either String Value
parseValue s t
  = either (Left . show)
           Right
           (parse (parseVal t) "" s)

 
