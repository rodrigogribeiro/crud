module Data.Parser ( module Text.Parsec
                   , module Data.Parser ) where

import Text.Parsec hiding (string)
import Text.Parsec.Language
import qualified Text.Parsec.Token as Tk
import Data.Functor.Identity

type Parser a = ParsecT String () Identity a

floatParser :: Parser Double
floatParser = Tk.float tokenParser

stringParser :: Parser String
stringParser = many anyChar

intParser :: Parser Integer
intParser = Tk.integer tokenParser

boolParser :: Parser Bool
boolParser = (const True <$> symbolParser "true") <|>
             (const False <$> symbolParser "false")

symbolParser :: String -> Parser String
symbolParser = Tk.symbol tokenParser 

tokenParser :: Tk.GenTokenParser String u Identity
tokenParser = Tk.makeTokenParser haskellDef
