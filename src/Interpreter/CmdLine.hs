{-# LANGUAGE OverloadedStrings #-}

module Interpreter.CmdLine where

import Data.Parser
import Data.Schema
import Data.Table
import Data.DB

type CMD a = IO (Either String a)


data Mode = Create | Read | Update | Delete | End
            deriving (Eq, Ord)


instance Show Mode where
  show Create = "I"
  show Read = "C"
  show Update = "A"
  show Delete = "R"
  show End = "E"


parseMode :: Parser Mode
parseMode
  = choice ((\ m -> m <$ symbolParser (show m)) <$> ms)
    where
      ms = [Create, Read, Update, Delete, End]


readMode :: IO Mode
readMode
  = do
      putStr "Escolha uma opção: (I)nserir, (C)onsultar, (A)tualizar, (R)emover, (E)ncerrar:"
      s <- getLine
      let m = parse parseMode "" s
      either (\ _ -> putStrLn "Opção inválida" >> readMode)
             return
             m

interpreter :: Schema -> DB -> CMD DB
interpreter sch db
  = do
      putStrLn ("CRUD " ++ (show (name sch)))
      m <- readMode
      case m of
        Create ->
          do
            db' <- insertLogic sch db
            either (return . Left)
                   (interpreter sch)
                   db'
        Read   -> queryLogic sch db 
        Update -> updateLogic sch db
        Delete -> removeLogic sch db
        End    -> return (Right db)


insertLogic :: Schema -> DB -> CMD DB
insertLogic sch db
  = do
      let nm = name sch
          fs = fields sch
          field fd = ((name fd),(fieldType fd))
      putStrLn ("Cadastro de " ++ (show nm))
      vs <- mapM (uncurry interpretField . field) fs
      let vals = [ v | (Right v) <- vs]
      return (Right (insert (Record vals) db))

queryLogic :: Schema -> DB -> CMD DB
queryLogic sch db = undefined

updateLogic :: Schema -> DB -> CMD DB
updateLogic sch db = undefined

removeLogic :: Schema -> DB -> CMD DB
removeLogic sch db = undefined


interpretField :: Name -> Type -> CMD (Name,Value)
interpretField n t
  = do
     putStr ((show n) ++ ":")
     s <- getLine
     either (return . Left)
            (\v -> return $ Right (n,v))
            (parseValue s t)
       

-- clear screen

clear :: IO ()
clear = putStr "\ESC[2J"

-- sample schema for testing

test :: Schema
test = ("Nome" .:. string)   .<>.
       ("Idade" .:. integer) .<>.
       emptySchema "Pessoa"
