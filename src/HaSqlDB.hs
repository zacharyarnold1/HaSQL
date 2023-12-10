module HaSqlDB where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified
import HaSqlSyntax
import State

type Column = String

type Name = String

newType Database = Database (Map Name Table)

data Table = Table [Column] [Record]

type Record = Map Column Value

displayTable :: Table -> IO ()
displayTable (Table cols records) = do
  print cols
  mapM_ print records

select :: ColumnObj -> String -> Clause -> State Database Table
select columnObj tableName clause = state $ \db ->
  case Data.Map.lookup tableName db of
    Just (Table cols records) ->
      let filteredRecords = filter (evalClause clause) records
       in (Table cols filteredRecords, db)
    Nothing -> (Table [] [], db)

create :: String -> [(String, Value)] -> State Database (Maybe String)
create tableName columns = state $ \db ->
  if Data.Map.member tableName db
    then (Nothing, db)
    else
      let newTable = Table (map fst columns) []
       in (Just tableName, Data.Map.insert tableName newTable db)

insert :: String -> [(String, Value)] -> State Database (Maybe String)
insert tableName record = state $ \db ->
  case Data.Map.lookup tableName db of
    Just (Table cols records) ->
      let newRecord = Data.Map.fromList record
       in (Just tableName, Data.Map.insert tableName (Table cols (newRecord : records)) db)
    Nothing -> (Nothing, db)

update :: String -> [(String, Value)] -> Clause -> State Database (Maybe String)
update tableName updates clause = state $ \db ->
  case Data.Map.lookup tableName db of
    Just (Table cols records) ->
      let updatedRecords = map (updateRecord updates clause) records
       in (Just tableName, Data.Map.insert tableName (Table cols updatedRecords) db)
    Nothing -> (Nothing, db)

delete :: String -> Clause -> State Database (Maybe String)
delete tableName clause = state $ \db ->
  case Data.Map.lookup tableName db of
    Just (Table cols records) ->
      let filteredRecords = filter (not . evalClause clause) records
       in (Just tableName, Data.Map.insert tableName (Table cols filteredRecords) db)
    Nothing -> (Nothing, db)

eval :: SQLObj -> State Database String
eval sqlObj = case sqlObj of
  SELECT colObj table whereClause ->
    do
      Table _ records <- select colObj table whereClause
      return $ show records
  CREATE table cols ->
    do
      result <- create table cols
      return $ maybe "Table creation failed" (const "Table created") result
  INSERT table record ->
    do
      result <- insert table record
      return $ maybe "Insert failed" (const "Record inserted") result
  UPDATE table updates whereClause ->
    do
      result <- update table updates whereClause
      return $ maybe "Update failed" (const "Records updated") result
  DELETE table whereClause ->
    do
      result <- delete table whereClause
      return $ maybe "Deletion failed" (const "Records deleted") result

createDB :: String -> Database
createDB _ = Database Data.Map.empty

evalClause :: Clause -> Record -> Bool
evalClause clause record = case clause of
  Clause val1 val2 op -> evalOp op (resolveValue val1) (resolveValue val2)
  AND c1 c2 -> evalClause c1 record && evalClause c2 record
  OR c1 c2 -> evalClause c1 record || evalClause c2 record
  NOT c -> not (evalClause c record)
  NONE -> True
  where
    resolveValue :: Value -> Value
    resolveValue v@(ColVal colName) = Data.Maybe.fromMaybe v (Data.Map.lookup colName record)
    resolveValue v = v
    evalOp :: ClauseOp -> Value -> Value -> Bool
    evalOp op v1 v2 = case op of
      EQ -> Data.Maybe.fromMaybe False (clauseEq v1 v2)
      NE -> Data.Maybe.fromMaybe False (clauseNeq v1 v2)
      LT -> Data.Maybe.fromMaybe False (clauseLe v1 v2)
      GT -> Data.Maybe.fromMaybe False (clauseGe v1 v2)
      LE -> Data.Maybe.fromMaybe False (clauseLeq v1 v2)
      GE -> Data.Maybe.fromMaybe False (clauseGeq v1 v2)
