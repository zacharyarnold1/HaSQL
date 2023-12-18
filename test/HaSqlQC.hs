module HaSqlQC where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import HaSqlDB (create, delete, evalClause, insert, recordsFromTable, select, update)
import HaSqlSyntax
import Test.QuickCheck

-- HaSqlDB
-- Properties
-- select only returns records that satisfy the clause

-- logicalNegation :: ClauseOp -> ClauseOp
-- logicalNegation EQS = NEQ
-- logicalNegation GEQ = LTH
-- logicalNegation LEQ = GTH
-- logicalNegation LTH = GEQ
-- logicalNegation NEQ = EQS
-- logicalNegation GTH = LEQ

-- prop_selectOnlySatisfying :: Table -> ClauseOp -> Bool
-- prop_selectOnlySatisfying (Table (Cols m) rcs) co = all (selectOnlySatisfyingHelper (Table (Cols m) rcs) co) (Map.toList m)

-- selectOnlySatisfyingHelper :: Table -> ClauseOp -> (String, ValType) -> Bool
-- selectOnlySatisfyingHelper t co (s, IntType) =
--   let clause = Clause (ColVal s) (IntVal 0) co
--       resultTable = evalState (select Star "test" clause) (databaseFromTable t)
--       satisfiesClause = all (evalClause clause) (recordsFromTable (fromMaybe (Table (Cols Map.empty) []) resultTable))
--    in satisfiesClause
-- selectOnlySatisfyingHelper t co (s, StringType) =
--   let clause = Clause (ColVal s) (StringVal "mama") co
--       resultTable = evalState (select Star "test" clause) (databaseFromTable t)
--       satisfiesClause = all (evalClause clause) (recordsFromTable (fromMaybe (Table (Cols Map.empty) []) resultTable))
--    in satisfiesClause

-- prop_selectOnlySatisfyingB :: Table -> ClauseOp -> Bool
-- prop_selectOnlySatisfyingB (Table (Cols m) rcs) co = all (selectOnlySatisfyingHelper (Table (Cols m) rcs) co) (Map.toList m)

-- selectOnlySatisfyingHelperB :: Table -> ClauseOp -> (String, ValType) -> Bool
-- selectOnlySatisfyingHelperB t co (s, IntType) =
--   let clause = Clause (ColVal s) (IntVal 0) co
--       clauseNeg = Clause (ColVal s) (IntVal 0) (logicalNegation co)
--       resultTable = evalState (select Star "test" clause) (databaseFromTable t)
--       satisfiesClause = not $ any (evalClause clauseNeg) (recordsFromTable (fromMaybe (Table (Cols Map.empty) []) resultTable))
--    in satisfiesClause
-- selectOnlySatisfyingHelperB t co (s, StringType) =
--   let clause = Clause (ColVal s) (StringVal "mama") co
--       clauseNeg = Clause (ColVal s) (StringVal "mama") (logicalNegation co)
--       resultTable = evalState (select Star "test" clause) (databaseFromTable t)
--       satisfiesClause = not $ any (evalClause clauseNeg) (recordsFromTable (fromMaybe (Table (Cols Map.empty) []) resultTable))
--    in satisfiesClause

-- Helper function to create a Database from a single Table for testing
-- databaseFromTable :: Table -> Database
-- databaseFromTable table = DB $ Map.fromList [("test", table)]

-- Helper function to extract records from a Table
-- recordsFromTable :: Table -> [Record]
-- recordsFromTable (Table _ records) = records

-- select only returns specified columns (unless given star)
-- prop_selectOnlySpecifiedColumns :: Table -> ColumnObj -> Bool
-- prop_selectOnlySpecifiedColumns table colObj =
--   let resultTable = evalState (select colObj "test" NONE) (databaseFromTable table)
--       expectedColumns = case colObj of
--         Star -> allColumns table
--         Columns cols -> cols
--       containsOnlyExpectedColumns = all (`elem` expectedColumns) (columnsFromTable (fromMaybe (Table (Cols Map.empty) []) resultTable))
--    in containsOnlyExpectedColumns

-- Helper function to get all column names from a table
-- allColumns :: Table -> [String]
-- allColumns (Table (Cols cols) _) = Map.keys cols

-- Helper function to get column names from a table
-- columnsFromTable :: Table -> [String]
-- columnsFromTable (Table (Cols cols) _) = Map.keys cols

prop_insertIncreasesSize :: Table -> Property
prop_insertIncreasesSize table = forAll (genMatchingRecord table) $ \record ->
  let tableSizeBefore = length (recordsFromTable table)
      updatedTable = execState (insert "tableName" record) (DB (Map.singleton "tableName" table))
      tableSizeAfter = length (recordsFromTable (fromJust (Map.lookup "tableName" (fromDatabase updatedTable))))
   in tableSizeAfter === tableSizeBefore + 1

prop_createTableSuccess :: Database -> String -> [(String, ValType)] -> Property
prop_createTableSuccess db tableName columns =
  not (Map.member tableName (fromDatabase db))
    ==> let (_, DB db') = runState (create tableName columns) db
         in Map.member tableName db' === True

prop_insertIntoNonExistingTable :: Database -> String -> [(String, Value)] -> Property
prop_insertIntoNonExistingTable db tableName record =
  not (Map.member tableName (fromDatabase db))
    ==> let (result, _) = runState (insert tableName record) db
         in isNothing result === True

prop_deleteFromNonExistingTable :: Database -> String -> Clause -> Property
prop_deleteFromNonExistingTable db tableName clause =
  not (Map.member tableName (fromDatabase db))
    ==> let (result, _) = runState (delete tableName clause) db
         in isNothing result === True

prop_updateNonExistingTable :: Database -> String -> [(String, Value)] -> Clause -> Property
prop_updateNonExistingTable db tableName updates clause =
  not (Map.member tableName (fromDatabase db))
    ==> let (result, _) = runState (update tableName updates clause) db
         in isNothing result === True

prop_createTableIncreasesDbSize :: Database -> String -> [(String, ValType)] -> Property
prop_createTableIncreasesDbSize db tableName columns =
  not (Map.member tableName (fromDatabase db))
    ==> let oldSize = Map.size (fromDatabase db)
            (_, DB db') = runState (create tableName columns) db
            newSize = Map.size db'
         in newSize === oldSize + 1

prop_multipleInsertions :: Database -> String -> [[(String, Value)]] -> Property
prop_multipleInsertions db tableName records =
  Map.member tableName (fromDatabase db)
    ==> let oldSize = length . recordsFromTable $ fromJust (Map.lookup tableName (fromDatabase db))
            db' = foldl (\db rec -> execState (insert tableName rec) db) db records
            newSize = length . recordsFromTable $ fromJust (Map.lookup tableName (fromDatabase db'))
         in newSize === oldSize + length records

prop_deletedRecordsAbsent :: Database -> Clause -> Property
prop_deletedRecordsAbsent db clause = not (Map.null (fromDatabase db))
  ==> forAll (tableNameGen db)
  $ \tableName ->
    let Just table = Map.lookup tableName (fromDatabase db)
        (_, DB db') = runState (delete tableName clause) db
        deletedRecords = filter (evalClause clause) (recordsFromTable table)
        remainingRecords = recordsFromTable (fromJust $ Map.lookup tableName db')
     in all (`notElem` deletedRecords) remainingRecords

prop_selectAllColumns :: Database -> Property
prop_selectAllColumns db = not (Map.null (fromDatabase db))
  ==> forAll (tableNameGen db)
  $ \tableName ->
    let tableObj = TName tableName
        (result, _) = runState (select Star tableObj Map.empty NONE [] (["id"], False)) db
        originalRecords = recordsFromTable (fromJust $ Map.lookup tableName (fromDatabase db))
     in fmap recordsFromTable result === Just originalRecords

prop_insertionIntegrity :: Database -> Property
prop_insertionIntegrity db = not (Map.null (fromDatabase db))
  ==> forAll (tableNameGen db)
  $ \tableName ->
    let Just table = Map.lookup tableName (fromDatabase db)
     in forAll (genMatchingRecord table) $ \record ->
          let (_, DB db') = runState (insert tableName record) db
              newRecords = recordsFromTable (fromJust $ Map.lookup tableName db')
           in any (matchesRecord record) newRecords
  where
    matchesRecord rec (Rec r) = all (\(col, val) -> Map.lookup col r == Just val) rec

prop_insertionIncreasesCount :: Database -> Property
prop_insertionIncreasesCount db = not (Map.null (fromDatabase db))
  ==> forAll (tableNameGen db)
  $ \tableName ->
    let Just table = Map.lookup tableName (fromDatabase db)
     in forAll (genMatchingRecord table) $ \record ->
          let oldCount = length . recordsFromTable $ fromJust (Map.lookup tableName (fromDatabase db))
              (_, DB db') = runState (insert tableName record) db
              newCount = length . recordsFromTable $ fromJust (Map.lookup tableName db')
           in newCount === oldCount + 1

prop_updateAllRecords :: Database -> [(String, Value)] -> Property
prop_updateAllRecords db updates = not (Map.null (fromDatabase db))
  ==> forAll (tableNameGen db)
  $ \tableName ->
    let Just table = Map.lookup tableName (fromDatabase db)
     in forAll (genMatchingRecord table) $ \record ->
          let (_, DB db') = runState (update tableName record NONE) db
              updatedRecords = recordsFromTable (fromJust $ Map.lookup tableName db')
           in all (updatedRecord record) updatedRecords
  where
    updatedRecord updates (Rec record) = all (\(col, val) -> Map.lookup col record == Just val) updates

prop_deleteEntireTableEmpty :: Database -> Property
prop_deleteEntireTableEmpty db = not (Map.null (fromDatabase db))
  ==> forAll (tableNameGen db)
  $ \tableName ->
    let (_, DB db') = runState (delete tableName NONE) db
        newTable = fromJust $ Map.lookup tableName db'
     in null (recordsFromTable newTable)

-- select should return nothing for tables that dont exist
-- create should create new table in database of specified name
-- create should return nothing if table by given name already exists
-- insert should add any valid record to a table
-- insert should not add non-valid records
-- update should only update records that satisfy a clause
-- num updated records should match num records that match clause
-- delete should only delete records that satisfy a clause
-- num deleted records should match num records that match clause
-- records should be ordered according to specified columns and direction
-- operations should return a new database state
-- tableToString should produce a consistent string representation for any table
-- Round trip
-- tableToString, convert to string and revert it back to table. check that its equal to original
-- inserting a record and then deleting it should yield original table, and vice versa
-- updating a record and then applying an inverse update to that record should revert back to original table

-- HaSqlJoins
-- Properties
-- inner join: for each pair of records that satisfies the predicate, there should be exactly one record corresponding to the combination of those two records
-- any record in a table that is the result of the inner join should satisfy the predicate
-- left join: every record in the left table should be in the resulting table, with either a matching right table record or a completly nil record
-- any record from the right table is in the left join result, it should satisfy the predicate
-- right join: every record in the right table should be in the resulting table, with either a matching left table record or a completly nil record
-- any record from the left table is in the right join result, it should satisfy the predicate
-- full join: every record from both tables should appear in the full join table
-- if a record from etiher table is in the full join, it either satisfies the predicate, or is filled with nil vals for the non matching table
-- natural join: the result should only include records where the values of commonly named columns are matching
-- the result should contain all columns from both tables

-- HaSqlFiles / LoadParser
-- Properties
-- Loading a nonexistent database should load DBLoad Nothing Nothing (file doesnt exist)
-- Roundtrip
-- Serializing a table and then deserializing it should yield original table
-- saving a database and then loading it should yield original database
