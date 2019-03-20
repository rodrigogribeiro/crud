{-# LANGUAGE OverloadedStrings #-}

module Data.Schema
       ( Type
       , Name
       , Field
       , (.:.)
       , Schema
       , emptySchema
       , addField
       , (.<>.)
       , fields
       , Nameable (..)
       , fieldType
       , integer
       , bool
       , string
       , float) where

import Data.String

-- definition of types

data Type = INTEGER | BOOL | STRING | FLOAT
            deriving (Eq, Ord, Show)

newtype Name = Name { unName :: String }
               deriving (Eq,Ord)

instance Show Name where
  show = unName

instance IsString Name where
  fromString = Name

class Nameable a where
  name :: a -> Name 

-- schema definitions

data Field
  = Field {
       fname :: Name
    ,  ftype :: Type
    } deriving (Eq, Ord, Show)

fieldType :: Field -> Type
fieldType = ftype

instance Nameable Field where
  name = fname

instance Nameable Schema where
  name = schname

infixr 6 .<>.

(.:.) :: Name -> Type -> Field
n .:. t = Field n t

data Schema
  = Schema {
      schname :: Name
    , fields  :: [Field]
    }
    deriving (Eq, Ord, Show)

emptySchema :: Name -> Schema
emptySchema n = Schema n []

addField :: Field -> Schema -> Schema
addField f (Schema n fs)
  = Schema n (f : fs)

(.<>.) :: Field -> Schema -> Schema
(.<>.) = addField

-- some smart constructors for these types

integer :: Type
integer = INTEGER

bool :: Type
bool = BOOL

string :: Type
string = STRING

float :: Type
float = FLOAT
