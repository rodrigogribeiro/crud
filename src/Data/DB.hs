module Data.DB ( DB
               , insert
               , queryBy
               , update
               , remove )where

import Data.Table


type DB = [Record]


insert :: Record -> DB -> DB
insert = (:)

-- first parameter: field projection
-- second parameter search value

queryBy :: (Record -> a) -> (a -> Bool) -> DB -> Maybe Record
queryBy prj p db
  = if null rs then Nothing else Just (head rs)
    where
      rs = [r | r <- db, p (prj r)]


-- first parameter: field projection
-- second parameter: search value

update :: (Record -> a) -> (a -> Bool) -> (Record -> Record) -> DB -> DB
update prj p f db
   = case queryBy prj p db of
       Just r  -> f r : [r' | r' <- db, r /= r'] 
       Nothing -> db

remove :: (Record -> a) -> (a -> Bool) -> DB -> DB
remove prj p db
  = [r' | r' <- db, not (p (prj r'))]  

