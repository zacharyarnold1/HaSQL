module HaSqlDB where

import Control.Monad.State
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Map (Map)
import Data.Map qualified
import Data.Maybe
import Data.Ord (Down (Down), comparing)
import HaSqlSyntax
import HaSqlTables

select :: ColumnObj -> TableObj -> Clause -> [String] -> ([String], Bool) -> State Database (Maybe Table)
select cs tableObj clause groupByCols (orderByCols, d) = state $ \(DB db) ->
  let table = evalTable tableObj db
      filteredRecords = filter (evalClause clause) (recordsFromTable table)
      selectedTable = selectHelper cs (Table (colsFromTable table) filteredRecords)
      groupedTable = groupOn groupByCols selectedTable
      orderedTable = HaSqlDB.orderBy orderByCols d groupedTable
   in (Just orderedTable, DB db)

colsFromTable :: Table -> Cols
colsFromTable (Table cols _) = cols

recordsFromTable :: Table -> [Record]
recordsFromTable (Table _ records) = records

selectHelper :: ColumnObj -> Table -> Table
selectHelper Star table = table
selectHelper (Columns cols) (Table allCols records) = Table (filterHeaders cols allCols) filteredRecords
  where
    filteredRecords = map (filterColumns cols) records
    filterColumns :: [String] -> Record -> Record
    filterColumns cols (Rec r) = Rec $ Data.Map.filterWithKey (\k _ -> k `elem` cols) r

    filterHeaders :: [String] -> Cols -> Cols
    filterHeaders hds (Cols c) = Cols $ Data.Map.filterWithKey (\k _ -> k `elem` hds) c

create :: String -> [(String, ValType)] -> State Database (Maybe String)
create tableName columns = state $ \(DB db) ->
  if Data.Map.member tableName db
    then (Nothing, DB db) -- return a message that this table already exists
    else
      let newTable = Table (Cols (Data.Map.fromList columns)) []
       in (Just tableName, DB $ Data.Map.insert tableName newTable db)

insert :: String -> [(String, Value)] -> State Database (Maybe String)
insert tableName record = state $ \(DB db) ->
  case Data.Map.lookup tableName db of
    Just (Table cols records) ->
      if validateType record cols
        then
          let newRecord = Rec $ Data.Map.fromList record
           in (Just tableName, DB $ Data.Map.insert tableName (Table cols (newRecord : records)) db)
        else (Nothing, DB db)
    Nothing -> (Nothing, DB db)

validateType :: [(String, Value)] -> Cols -> Bool
validateType [] cs = True
validateType ((s, v) : xs) cs = valHelper (Data.Map.lookup s (fromCols cs)) v cs && validateType xs cs
  where
    valHelper :: Maybe ValType -> Value -> Cols -> Bool
    valHelper Nothing _ _ = False
    valHelper (Just IntType) (IntVal i) _ = True
    valHelper (Just StringType) (StringVal s) _ = True
    valHelper _ NilVal _ = True
    valHelper (Just t) (ColVal c) cs = case Data.Map.lookup c (fromCols cs) of
      Just t2 -> t == t2
      Nothing -> False
    valHelper _ _ _ = False

update :: String -> [(String, Value)] -> Clause -> State Database (Maybe String)
update tableName updates clause = state $ \(DB db) ->
  case Data.Map.lookup tableName db of
    Just (Table cols records) ->
      if validateType updates cols
        then
          let updatedRecords = map (updateRecord updates clause) records
           in (Just tableName, DB $ Data.Map.insert tableName (Table cols updatedRecords) db)
        else (Nothing, DB db)
    Nothing -> (Nothing, DB db)

updateRecord :: [(String, Value)] -> Clause -> Record -> Record
updateRecord updates clause record = case evalClause clause record of
  False -> record
  True -> foldr (updateCol . recHelper record) record updates
    where
      updateCol :: (String, Value) -> Record -> Record
      updateCol (n, v) (Rec r) = Rec $ Data.Map.insert n v r

recHelper :: Record -> (String, Value) -> (String, Value)
recHelper (Rec record) (s, ColVal c) = case Data.Map.lookup c record of
  Nothing -> (s, NilVal)
  Just v -> (s, v)
recHelper _ sv = sv

delete :: String -> Clause -> State Database (Maybe String)
delete tableName clause = state $ \(DB db) ->
  case Data.Map.lookup tableName db of
    Just (Table cols records) ->
      let filteredRecords = filter (not . evalClause clause) records
       in (Just tableName, DB $ Data.Map.insert tableName (Table cols filteredRecords) db)
    Nothing -> (Nothing, DB db)

compareValue :: Value -> Value -> Ordering
compareValue NilVal NilVal = EQ
compareValue NilVal _ = LT
compareValue _ NilVal = GT
compareValue (IntVal a) (IntVal b) = compare a b
compareValue (StringVal a) (StringVal b) = compare a b
compareValue _ _ = error "Cannot compare different types"

compareRecords :: [String] -> Record -> Record -> Ordering
compareRecords cols rec1 rec2 =
  mconcat [compareValue (fromMaybe NilVal $ Data.Map.lookup col (fromRecord rec1)) (fromMaybe NilVal $ Data.Map.lookup col (fromRecord rec2)) | col <- cols]

orderBy :: [String] -> Bool -> Table -> Table
orderBy [] _ table = table
orderBy cols ascending (Table tableCols records) =
  let comparator = if ascending then compareRecords cols else flip (compareRecords cols)
   in Table tableCols (sortBy comparator records)

groupOn :: [String] -> Table -> Table
groupOn groupByCols (Table (Cols cols) records) =
  let groupedRecords =
        groupBy (\r1 r2 -> allEqual r1 r2 groupByCols) $
          sortBy (compareRecords groupByCols) records
      newRecords = map createGroupRecord groupedRecords
   in Table (Cols cols) newRecords

allEqual :: Record -> Record -> [String] -> Bool
allEqual rec1 rec2 = all (\col -> Data.Map.lookup col (fromRecord rec1) == Data.Map.lookup col (fromRecord rec2))

createGroupRecord :: [Record] -> Record
createGroupRecord records =
  case records of
    (r : _) -> r
    [] -> error "Empty group encountered"

eval :: SQLObj -> State Database String
eval sqlObj = case sqlObj of
  SELECT colObj table whereClause grouping order ->
    do
      result <- select colObj table whereClause grouping order
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

evalClause :: Clause -> Record -> Bool
evalClause clause record = case clause of
  Clause val1 val2 op -> evalOp op (resolveValue val1) (resolveValue val2)
  AND c1 c2 -> evalClause c1 record && evalClause c2 record
  OR c1 c2 -> evalClause c1 record || evalClause c2 record
  NOT c -> not (evalClause c record)
  NONE -> True
  where
    resolveValue :: Value -> Value
    resolveValue v@(ColVal colName) = Data.Maybe.fromMaybe v (Data.Map.lookup colName (fromRecord record))
    resolveValue v = v
    evalOp :: ClauseOp -> Value -> Value -> Bool
    evalOp op v1 v2 = case op of
      EQS -> Data.Maybe.fromMaybe False (clauseEq v1 v2)
      NEQ -> Data.Maybe.fromMaybe False (clauseNeq v1 v2)
      LTH -> Data.Maybe.fromMaybe False (clauseLe v1 v2)
      GTH -> Data.Maybe.fromMaybe False (clauseGe v1 v2)
      LEQ -> Data.Maybe.fromMaybe False (clauseLeq v1 v2)
      GEQ -> Data.Maybe.fromMaybe False (clauseGeq v1 v2)

evalTable :: TableObj -> Map Name Table -> Table
evalTable (TName name) db = Data.Maybe.fromMaybe (error "Table not found: ") (Data.Map.lookup name db)
evalTable (CTE (SELECT colObj table whereClause grouping order)) db = fromMaybe (Table (Cols Data.Map.empty) []) $ evalState (select colObj table whereClause grouping order) $ DB db
evalTable (CTE _) _ = undefined
evalTable (INNERJOIN obj1 obj2 pairs) db = innerJoin pairs (evalTable obj1 db) (evalTable obj2 db)
evalTable (LEFTJOIN obj1 obj2 pairs) db = leftJoin pairs (evalTable obj1 db) (evalTable obj2 db)
evalTable (RIGHTJOIN obj1 obj2 pairs) db = rightJoin pairs (evalTable obj1 db) (evalTable obj2 db)
evalTable (FULLJOIN obj1 obj2 pairs) db = fullJoin pairs (evalTable obj1 db) (evalTable obj2 db)
evalTable (NATURALJOIN obj1 obj2) db = naturalJoin (evalTable obj1 db) (evalTable obj2 db)