module HaSqlDBSelect where

import Control.Monad.State
import Data.List (sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import HaSqlClauseLogic
import HaSqlLoadParser
import HaSqlSyntax
import HaSqlTables

-- simulates SELECT SQL command, takes in the contents of a SELECT SQLObj, and returns a table containing the query results
select :: ColumnObj -> TableObj -> Map String SQLObj -> Clause -> ([String], Bool) -> State Database (Maybe Table)
select cs tableObj selMap clause (orderByCols, d) = state $ \(DB db) ->
  let modTableObj = evalCTEs tableObj selMap
      table = evalTable modTableObj selMap db
      filteredRecords = filter (evalClause clause) (recordsFromTable table)
      selectedTable = selectHelper cs (Table (colsFromTable table) filteredRecords)
      orderedTable = orderOn orderByCols d selectedTable
   in (Just orderedTable, DB db)

-- helper to select, handles optional list of common table expressions that preceed the query
evalCTEs :: TableObj -> Map String SQLObj -> TableObj
evalCTEs (TName s) m =
  case Map.lookup s m of
    Just so -> CTE so
    Nothing -> TName s
evalCTEs (CTE so) _ = CTE so
evalCTEs (INNERJOIN to1 to2 con) m = INNERJOIN (evalCTEs to1 m) (evalCTEs to2 m) con
evalCTEs (LEFTJOIN to1 to2 con) m = LEFTJOIN (evalCTEs to1 m) (evalCTEs to2 m) con
evalCTEs (RIGHTJOIN to1 to2 con) m = RIGHTJOIN (evalCTEs to1 m) (evalCTEs to2 m) con
evalCTEs (FULLJOIN to1 to2 con) m = FULLJOIN (evalCTEs to1 m) (evalCTEs to2 m) con
evalCTEs (NATURALJOIN to1 to2) m = NATURALJOIN (evalCTEs to1 m) (evalCTEs to2 m)

-- helper for select, condenses table such that only appropriate columns are returned
selectHelper :: ColumnObj -> Table -> Table
selectHelper Star table = table
selectHelper (Columns cols) (Table allCols records) = Table (filterHeaders cols allCols) filteredRecords
  where
    filteredRecords = map (filterColumns cols) records
    filterColumns :: [String] -> Record -> Record
    filterColumns cols (Rec r) = Rec $ Map.filterWithKey (\k _ -> k `elem` cols) r

    filterHeaders :: [String] -> Cols -> Cols
    filterHeaders hds (Cols c) = Cols $ Map.filterWithKey (\k _ -> k `elem` hds) c

-- orders a table over the given columns, if bool is true, order is ascedning, else descending. returns ordered table
orderOn :: [String] -> Bool -> Table -> Table
orderOn [] _ table = table
orderOn cols ascending (Table tableCols records) =
  let comparator = if ascending then compareRecords cols else flip (compareRecords cols)
   in Table tableCols (sortBy comparator records)

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
  mconcat [compareValue (fromMaybe NilVal $ Map.lookup col (fromRecord rec1)) (fromMaybe NilVal $ Map.lookup col (fromRecord rec2)) | col <- cols]

-- resolves a TableObj - performs appropriate joins, evaluates common table expression, or fetches the correct table from database (base case)
evalTable :: TableObj -> Map String SQLObj -> Map Name Table -> Table
evalTable (TName name) m db = Data.Maybe.fromMaybe (error "Table not found: ") (Map.lookup name db)
evalTable (CTE (SELECT colObj table whereClause order)) m db = fromMaybe (Table (Cols Map.empty) []) $ evalState (select colObj table m whereClause order) $ DB db
evalTable (CTE _) _ _ = undefined
evalTable (INNERJOIN obj1 obj2 pairs) m db = innerJoin pairs (evalTable obj1 m db) (evalTable obj2 m db)
evalTable (LEFTJOIN obj1 obj2 pairs) m db = leftJoin pairs (evalTable obj1 m db) (evalTable obj2 m db)
evalTable (RIGHTJOIN obj1 obj2 pairs) m db = rightJoin pairs (evalTable obj1 m db) (evalTable obj2 m db)
evalTable (FULLJOIN obj1 obj2 pairs) m db = fullJoin pairs (evalTable obj1 m db) (evalTable obj2 m db)
evalTable (NATURALJOIN obj1 obj2) m db = naturalJoin (evalTable obj1 m db) (evalTable obj2 m db)