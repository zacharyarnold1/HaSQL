module HaSqlDBRecordOps where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import HaSqlClauseLogic
import HaSqlSyntax

-- inserts a record (list of string, value tuples) into specified table
insert :: String -> [(String, Value)] -> State Database (Maybe String)
insert tableName record = state $ \(DB db) ->
  case Map.lookup tableName db of
    Just (Table cols records) ->
      if validateType record cols
        then
          let newRecord = Rec $ Map.fromList record
           in (Just tableName, DB $ Map.insert tableName (Table cols (newRecord : records)) db)
        else (Nothing, DB db)
    Nothing -> (Nothing, DB db)

-- updates record in given table based on given clause. validates the type of the record with the table and calls appropriate helper
update :: String -> [(String, Value)] -> Clause -> State Database (Maybe String)
update tableName updates clause = state $ \(DB db) ->
  case Map.lookup tableName db of
    Just (Table cols records) ->
      if validateType updates cols
        then
          let updatedRecords = map (updateRecord updates clause) records
           in (Just tableName, DB $ Map.insert tableName (Table cols updatedRecords) db)
        else (Nothing, DB db)
    Nothing -> (Nothing, DB db)

-- update helper, updates individiual record if it satisfies given clause, returns updated record or original record if not updated
updateRecord :: [(String, Value)] -> Clause -> Record -> Record
updateRecord updates clause record = case evalClause clause record of
  False -> record
  True -> foldr (updateCol . resolveCol record) record updates
    where
      updateCol :: (String, Value) -> Record -> Record
      updateCol (n, v) (Rec r) = Rec $ Map.insert n v r

-- updateRecord helper, resolves column values in updates
resolveCol :: Record -> (String, Value) -> (String, Value)
resolveCol (Rec record) (s, ColVal c) = case Map.lookup c record of
  Nothing -> (s, NilVal)
  Just v -> (s, v)
resolveCol _ sv = sv

-- checks that a record matches the type of the columns of a table
validateType :: [(String, Value)] -> Cols -> Bool
validateType [] cs = True
validateType ((s, v) : xs) cs = valHelper (Map.lookup s (fromCols cs)) v cs && validateType xs cs
  where
    valHelper :: Maybe ValType -> Value -> Cols -> Bool
    valHelper Nothing _ _ = False
    valHelper (Just IntType) (IntVal i) _ = True
    valHelper (Just StringType) (StringVal s) _ = True
    valHelper _ NilVal _ = True
    valHelper (Just t) (ColVal c) cs = case Map.lookup c (fromCols cs) of
      Just t2 -> t == t2
      Nothing -> False
    valHelper _ _ _ = False

-- deletes any record matching given clause in the given table
delete :: String -> Clause -> State Database (Maybe String)
delete tableName clause = state $ \(DB db) ->
  case Map.lookup tableName db of
    Just (Table cols records) ->
      let filteredRecords = filter (not . evalClause clause) records
       in (Just tableName, DB $ Map.insert tableName (Table cols filteredRecords) db)
    Nothing -> (Nothing, DB db)
