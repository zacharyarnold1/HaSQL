module HaSqlDB where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified
import Data.Maybe
import HaSqlSyntax

type Cols = Map String ValType

type Name = String

type Database = Map Name Table

data Table = Table Cols [Record]

type Record = Map String Value

select :: ColumnObj -> String -> Clause -> State Database (Maybe Table)
select cs tableName clause = state $ \db ->
  case Data.Map.lookup tableName db of
    Just (Table cols records) ->
      let filteredRecords = filter (evalClause clause) records
       in (Just (selectHelper cs (Table cols filteredRecords)), db)
    Nothing -> (Nothing, db)

selectHelper :: ColumnObj -> Table -> Table
selectHelper Star table = table
selectHelper (Columns cols) (Table allCols records) = Table (filterHeaders cols allCols) filteredRecords
  where
    filteredRecords = map (filterColumns cols) records
    filterColumns :: [String] -> Record -> Record
    filterColumns cols = Data.Map.filterWithKey (\k _ -> k `elem` cols)

    filterHeaders :: [String] -> Cols -> Cols
    filterHeaders hds = Data.Map.filterWithKey (\k _ -> k `elem` hds)

create :: String -> [(String, ValType)] -> State Database (Maybe String)
create tableName columns = state $ \db ->
  if Data.Map.member tableName db
    then (Nothing, db) -- return a message that this table already exists
    else
      let newTable = Table (Data.Map.fromList columns) []
       in (Just tableName, Data.Map.insert tableName newTable db)

insert :: String -> [(String, Value)] -> State Database (Maybe String)
insert tableName record = state $ \db ->
  case Data.Map.lookup tableName db of
    Just (Table cols records) ->
      if validateType record cols
        then
          let newRecord = Data.Map.fromList record
           in (Just tableName, Data.Map.insert tableName (Table cols (newRecord : records)) db)
        else (Nothing, db)
    Nothing -> (Nothing, db)

validateType :: [(String, Value)] -> Cols -> Bool
validateType [] cs = True
validateType ((s, v) : xs) cs = valHelper (Data.Map.lookup s cs) v cs && validateType xs cs
  where
    valHelper :: Maybe ValType -> Value -> Cols -> Bool
    valHelper Nothing _ _ = False
    valHelper (Just IntType) (IntVal i) _ = True
    valHelper (Just StringType) (StringVal s) _ = True
    valHelper _ NilVal _ = True
    valHelper (Just t) (ColVal c) cs = case Data.Map.lookup c cs of
      Just t2 -> t == t2
      Nothing -> False
    valHelper _ _ _ = False

update :: String -> [(String, Value)] -> Clause -> State Database (Maybe String)
update tableName updates clause = state $ \db ->
  case Data.Map.lookup tableName db of
    Just (Table cols records) ->
      if validateType updates cols
        then
          let updatedRecords = map (updateRecord updates clause) records
           in (Just tableName, Data.Map.insert tableName (Table cols updatedRecords) db)
        else (Nothing, db)
    Nothing -> (Nothing, db)

updateRecord :: [(String, Value)] -> Clause -> Record -> Record
updateRecord updates clause record = case evalClause clause record of
  False -> record
  True -> foldr (updateCol . recHelper record) record updates
    where
      updateCol :: (String, Value) -> Record -> Record
      updateCol (n, v) = Data.Map.insert n v

recHelper :: Record -> (String, Value) -> (String, Value)
recHelper record (s, ColVal c) = case Data.Map.lookup c record of
  Nothing -> (s, NilVal)
  Just v -> (s, v)
recHelper _ sv = sv

delete :: String -> Clause -> State Database (Maybe String)
delete tableName clause = state $ \db ->
  case Data.Map.lookup tableName db of
    Just (Table cols records) ->
      let filteredRecords = filter (not . evalClause clause) records
       in (Just tableName, Data.Map.insert tableName (Table cols filteredRecords) db)
    Nothing -> (Nothing, db)

eval :: SQLObj -> State Database String
eval sqlObj = case sqlObj of
  SELECT colObj tableName whereClause ->
    do
      result <- select colObj tableName whereClause
      return $ maybe "No such table" tableToString result
  CREATE tableName header ->
    do
      result <- create tableName header
      return $ maybe "Table creation failed" (const "Table created") result
  INSERT tableName record ->
    do
      result <- insert tableName record
      return $ maybe "Insert failed" (const "Record inserted") result
  UPDATE tableName record whereClause ->
    do
      result <- update tableName record whereClause
      return $ maybe "Update failed" (const "Records updated") result
  DELETE tableName whereClause ->
    do
      result <- delete tableName whereClause
      return $ maybe "Deletion failed" (const "Records deleted") result

valueToString :: Value -> String
valueToString NilVal = "nil"
valueToString (IntVal i) = show i
valueToString (StringVal s) = "'" ++ s ++ "'"
valueToString (ColVal c) = c

valTypeToString :: ValType -> String
valTypeToString IntType = "int"
valTypeToString StringType = "str"

tableToString :: Table -> String
tableToString (Table cols records) = unlines $ header : map showRecord records
  where
    colNames = Data.Map.toList cols
    header = unwords $ map (\(name, t) -> name ++ " (" ++ valTypeToString t ++ ") |") colNames
    showRecord :: Record -> String
    showRecord record = unwords $ map (\(name, _) -> maybe "nil" valueToString (Data.Map.lookup name record) ++ " |") colNames

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
      EQS -> Data.Maybe.fromMaybe False (clauseEq v1 v2)
      NEQ -> Data.Maybe.fromMaybe False (clauseNeq v1 v2)
      LTH -> Data.Maybe.fromMaybe False (clauseLe v1 v2)
      GTH -> Data.Maybe.fromMaybe False (clauseGe v1 v2)
      LEQ -> Data.Maybe.fromMaybe False (clauseLeq v1 v2)
      GEQ -> Data.Maybe.fromMaybe False (clauseGeq v1 v2)
