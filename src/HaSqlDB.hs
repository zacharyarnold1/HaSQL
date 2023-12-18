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

-- helper to select, handles optional list of common table expressions that preceed the query
selectCTEhelper :: TableObj -> Map String SQLObj -> TableObj
selectCTEhelper (TName s) m =
  case Data.Map.lookup s m of
    Just so -> CTE so
    Nothing -> TName s
selectCTEhelper (CTE so) _ = CTE so
selectCTEhelper (INNERJOIN to1 to2 con) m = INNERJOIN (selectCTEhelper to1 m) (selectCTEhelper to2 m) con
selectCTEhelper (LEFTJOIN to1 to2 con) m = LEFTJOIN (selectCTEhelper to1 m) (selectCTEhelper to2 m) con
selectCTEhelper (RIGHTJOIN to1 to2 con) m = RIGHTJOIN (selectCTEhelper to1 m) (selectCTEhelper to2 m) con
selectCTEhelper (FULLJOIN to1 to2 con) m = FULLJOIN (selectCTEhelper to1 m) (selectCTEhelper to2 m) con
selectCTEhelper (NATURALJOIN to1 to2) m = NATURALJOIN (selectCTEhelper to1 m) (selectCTEhelper to2 m)

-- simulates SELECT SQL command, takes in the contents of a SELECT SQLObj, and returns a table containing the query results
select :: ColumnObj -> TableObj -> Map String SQLObj -> Clause -> ([String], Bool) -> State Database (Maybe Table)
select cs tableObj selMap clause (orderByCols, d) = state $ \(DB db) ->
  let modTableObj = selectCTEhelper tableObj selMap
      table = evalTable modTableObj selMap db
      filteredRecords = filter (evalClause clause) (recordsFromTable table)
      selectedTable = selectHelper cs (Table (colsFromTable table) filteredRecords)
      orderedTable = orderOn orderByCols d selectedTable
   in (Just orderedTable, DB db)

-- returns the columns (attributes with column names and type) from an inputted table
colsFromTable :: Table -> Cols
colsFromTable (Table cols _) = cols

-- returns all the records from an inputted table
recordsFromTable :: Table -> [Record]
recordsFromTable (Table _ records) = records

-- helper for select, condenses table such that only appropriate columns are returned
selectHelper :: ColumnObj -> Table -> Table
selectHelper Star table = table
selectHelper (Columns cols) (Table allCols records) = Table (filterHeaders cols allCols) filteredRecords
  where
    filteredRecords = map (filterColumns cols) records
    filterColumns :: [String] -> Record -> Record
    filterColumns cols (Rec r) = Rec $ Data.Map.filterWithKey (\k _ -> k `elem` cols) r

    filterHeaders :: [String] -> Cols -> Cols
    filterHeaders hds (Cols c) = Cols $ Data.Map.filterWithKey (\k _ -> k `elem` hds) c

-- creates a table
create :: String -> [(String, ValType)] -> State Database (Maybe String)
create tableName columns = state $ \(DB db) ->
  if Data.Map.member tableName db
    then (Nothing, DB db) -- return a message that this table already exists
    else
      let newTable = Table (Cols (Data.Map.fromList columns)) []
       in (Just tableName, DB $ Data.Map.insert tableName newTable db)

-- returns all table names and their schema in the database (VIEW)

-- inserts a record (list of string, value tuples) into specified table
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

-- deletes a table of specified name from the database
drop :: String -> State Database (Maybe String)
drop tableName = state $ \(DB db) ->
  if Data.Map.member tableName db
    then (Just tableName, DB $ Data.Map.delete tableName db)
    else (Nothing, DB db)

-- checks that a record matches the type of the columns of a table
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

-- updates record in given table based on given clause. validates the type of the record with the table and calls appropriate helper
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

-- update helper, updates individiual record if it satisfies given clause, returns updated record or original record if not updated
updateRecord :: [(String, Value)] -> Clause -> Record -> Record
updateRecord updates clause record = case evalClause clause record of
  False -> record
  True -> foldr (updateCol . recHelper record) record updates
    where
      updateCol :: (String, Value) -> Record -> Record
      updateCol (n, v) (Rec r) = Rec $ Data.Map.insert n v r

-- updateRecord helper, resolves column values in updates
recHelper :: Record -> (String, Value) -> (String, Value)
recHelper (Rec record) (s, ColVal c) = case Data.Map.lookup c record of
  Nothing -> (s, NilVal)
  Just v -> (s, v)
recHelper _ sv = sv

-- adds a column of specified name and type to a table of specified name
add :: [(String, ValType)] -> String -> State Database (Maybe String)
add newCols tableName = state $ \(DB db) ->
  case Data.Map.lookup tableName db of
    Just (Table (Cols cols) records) ->
      let updatedCols = foldr addColumn cols newCols
          addColumn (newCol, colType) acc =
            if Data.Map.member newCol acc
              then acc
              else Data.Map.insert newCol colType acc
       in if updatedCols /= cols
            then (Just tableName, DB $ Data.Map.insert tableName (Table (Cols updatedCols) records) db)
            else (Nothing, DB db)
    Nothing -> (Nothing, DB db)

renameTable :: String -> String -> State Database (Maybe String)
renameTable oldName newName = state $ \(DB db) ->
  case Data.Map.lookup oldName db of
    Just table ->
      (Just newName, DB $ Data.Map.insert newName table $ Data.Map.delete oldName db)
    Nothing ->
      (Nothing, DB db)

-- renames a column in a table of a specified name
renameCol :: String -> String -> String -> State Database (Maybe String)
renameCol newName oldName tableName = state $ \(DB db) ->
  case Data.Map.lookup tableName db of
    Just (Table (Cols cols) records) ->
      case Data.Map.lookup oldName cols of
        Nothing -> (Nothing, DB db) -- Old column name does not exist
        Just colType ->
          let newCols = Cols $ Data.Map.insert newName colType $ Data.Map.delete oldName cols
              newRecords = map (renameRecord oldName newName) records
              newTable = Table newCols newRecords
              newDb = DB $ Data.Map.insert tableName newTable db
           in (Just newName, newDb)
    Nothing -> (Nothing, DB db) -- Table does not exist

renameRecord :: String -> String -> Record -> Record
renameRecord oldName newName (Rec record) =
  let maybeVal = Data.Map.lookup oldName record
      recordWithoutOld = Data.Map.delete oldName record
   in Rec $ maybe recordWithoutOld (\val -> Data.Map.insert newName val recordWithoutOld) maybeVal

-- deletes any record matching given clause in the given table
delete :: String -> Clause -> State Database (Maybe String)
delete tableName clause = state $ \(DB db) ->
  case Data.Map.lookup tableName db of
    Just (Table cols records) ->
      let filteredRecords = filter (not . evalClause clause) records
       in (Just tableName, DB $ Data.Map.insert tableName (Table cols filteredRecords) db)
    Nothing -> (Nothing, DB db)

-- returns the ordering between two values, hard codes NilVal to be less than any other ValType
compareValue :: Value -> Value -> Ordering
compareValue NilVal NilVal = EQ
compareValue NilVal _ = LT
compareValue _ NilVal = GT
compareValue (IntVal a) (IntVal b) = compare a b
compareValue (StringVal a) (StringVal b) = compare a b
compareValue _ _ = error "Cannot compare different types"

-- determines the correct ordering between two records over the given list of columns
compareRecords :: [String] -> Record -> Record -> Ordering
compareRecords cols rec1 rec2 =
  mconcat [compareValue (fromMaybe NilVal $ Data.Map.lookup col (fromRecord rec1)) (fromMaybe NilVal $ Data.Map.lookup col (fromRecord rec2)) | col <- cols]

-- orders a table over the given columns, if bool is true, order is ascedning, else descending. returns ordered table
orderOn :: [String] -> Bool -> Table -> Table
orderOn [] _ table = table
orderOn cols ascending (Table tableCols records) =
  let comparator = if ascending then compareRecords cols else flip (compareRecords cols)
   in Table tableCols (sortBy comparator records)

-- executes the appropriate action on the inputted SQLObj, and returns either an error or success message
eval :: (SQLObj, [(String, SQLObj)]) -> State Database String
eval (sqlObj, l) =
  let m = Data.Map.fromList l
   in case sqlObj of
        SELECT colObj table whereClause order ->
          do
            result <- select colObj table m whereClause order
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
        DROP tableName ->
          do
            result <- HaSqlDB.drop tableName
            return $ maybe "No table found" (const "Table deleted") result
        ADD tableName newCols ->
          do
            result <- add newCols tableName
            return $ maybe "Add Column failed" (const "Column(s) added") result
        RENAMECOL tableName oldColName newColName ->
          do
            result <- renameCol newColName oldColName tableName
            return $ maybe "Rename Column failed" (const "Column renamed") result
        RENAMETABLE tableName newTableName ->
          do
            result <- renameTable tableName newTableName
            return $ maybe "Rename Table failed" (const "Table renamed") result
        VIEW -> view

-- given a clause and record, returns wether or not the record matches the clause
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

-- resolves a TableObj - performs appropriate joins, evaluates common table expression, or fetches the correct table from database (base case)
evalTable :: TableObj -> Map String SQLObj -> Map Name Table -> Table
evalTable (TName name) m db = Data.Maybe.fromMaybe (error "Table not found: ") (Data.Map.lookup name db)
evalTable (CTE (SELECT colObj table whereClause order)) m db = fromMaybe (Table (Cols Data.Map.empty) []) $ evalState (select colObj table m whereClause order) $ DB db
evalTable (CTE _) _ _ = undefined
evalTable (INNERJOIN obj1 obj2 pairs) m db = innerJoin pairs (evalTable obj1 m db) (evalTable obj2 m db)
evalTable (LEFTJOIN obj1 obj2 pairs) m db = leftJoin pairs (evalTable obj1 m db) (evalTable obj2 m db)
evalTable (RIGHTJOIN obj1 obj2 pairs) m db = rightJoin pairs (evalTable obj1 m db) (evalTable obj2 m db)
evalTable (FULLJOIN obj1 obj2 pairs) m db = fullJoin pairs (evalTable obj1 m db) (evalTable obj2 m db)
evalTable (NATURALJOIN obj1 obj2) m db = naturalJoin (evalTable obj1 m db) (evalTable obj2 m db)

view :: State Database String
view = state $ \(DB db) ->
  let schemas =
        if Data.Map.null db
          then "No tables in the database."
          else concatMap tableSchema $ Data.Map.toList db
   in (schemas, DB db)
  where
    tableSchema :: (String, Table) -> String
    tableSchema (name, Table (Cols cols) _) =
      "{"
        ++ name
        ++ "}\n"
        ++ "Schema:\n"
        ++ concatMap showCol (Data.Map.toList cols)
        ++ "\n"
    showCol :: (String, ValType) -> String
    showCol (colName, colType) = colName ++ " - " ++ valTypeToString colType ++ "\n"