module HaSqlQC where

import Control.Monad.State
import Data.Bool (Bool (False))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import HaSqlArbitrary
import HaSqlClauseLogic
import HaSqlDBOps
import HaSqlDBRecordOps
import HaSqlDBSelect
import HaSqlMainParser
import HaSqlPrettyPrint
import HaSqlSyntax
import Test.QuickCheck

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
        (result, _) = runState (select Star tableObj Map.empty NONE (["id"], False)) db
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

prop_dropTableReducesDbSize :: Database -> Property
prop_dropTableReducesDbSize db =
  forAll (tableNameGen db) $ \tableName ->
    let oldSize = Map.size (fromDatabase db)
        (_, DB db') = runState (HaSqlDBOps.drop tableName) db
        newSize = Map.size db'
     in newSize === oldSize - 1

prop_addColumnIncreasesSchemaSize :: Database -> String -> ValType -> Property
prop_addColumnIncreasesSchemaSize db tableName newColType =
  forAll (tableNameGen db) $ \tableName ->
    let oldSchemaSize = Map.size . fromCols . colsFromTable $ fromJust (Map.lookup tableName (fromDatabase db))
        newColName = "newCol"
        (_, DB db') = runState (add [(newColName, newColType)] tableName) db
        newSchemaSize = Map.size . fromCols . colsFromTable $ fromJust (Map.lookup tableName db')
     in newSchemaSize === oldSchemaSize + 1

prop_renameTableUpdatesName :: Database -> Property
prop_renameTableUpdatesName db =
  forAll (tableNameGen db) $ \oldName ->
    let newName = oldName ++ "_new"
        (_, DB db') = runState (renameTable oldName newName) db
     in Map.member newName db' && not (Map.member oldName db')

prop_updateInvalidRecordFails :: Database -> Table -> Property
prop_updateInvalidRecordFails db table =
  forAll (genMatchingRecord table) $ \record ->
    let invalidRecord = ("invalidCol", NilVal) : record
        tableName = "someTable"
        (result, _) = runState (update tableName invalidRecord NONE) db
     in isNothing result

prop_createDuplicateTableFails :: Database -> String -> [(String, ValType)] -> Bool
prop_createDuplicateTableFails db tableName columns =
  let (_, DB db') = runState (create tableName columns) db
      (result, _) = runState (create tableName columns) (DB db')
   in isNothing result

prop_deleteAllMakesTableEmpty :: Database -> Property
prop_deleteAllMakesTableEmpty db =
  forAll (tableNameGen db) $ \tableName ->
    let (_, DB db') = runState (delete tableName NONE) db
        table = fromJust $ Map.lookup tableName db'
     in null (recordsFromTable table)

prop_addColumnPreservesRecordCount :: Database -> String -> [(String, ValType)] -> Property
prop_addColumnPreservesRecordCount db tableName newCols =
  forAll (tableNameGen db) $
    \tableName ->
      let Just table = Map.lookup tableName (fromDatabase db)
          oldRecordCount = length (recordsFromTable table)
          (_, DB db') = runState (add newCols tableName) db
          newRecordCount = length . recordsFromTable $ fromJust (Map.lookup tableName db')
       in newRecordCount === oldRecordCount

prop_renameColumnUpdatesSchema :: Database -> String -> Property
prop_renameColumnUpdatesSchema db newColName =
  forAll (tableNameGen db) $
    \tableName ->
      forAll (columnNameGen $ fromJust $ Map.lookup tableName (fromDatabase db)) $
        \oldColName ->
          let (_, DB db') = runState (renameCol newColName oldColName tableName) db
              newTable = fromJust $ Map.lookup tableName db'
           in isJust (Map.lookup newColName (fromCols $ colsFromTable newTable)) && isNothing (Map.lookup oldColName (fromCols $ colsFromTable newTable))

prop_insertInvalidTypeFails :: Database -> Property
prop_insertInvalidTypeFails db =
  forAll (tableNameGen db) $
    \tableName ->
      forAll (genNotMatchingRecord $ fromJust $ Map.lookup tableName (fromDatabase db)) $
        \record ->
          let (result, _) = runState (insert tableName record) db
           in isNothing result
  where
    matchesType :: Value -> ValType -> Bool
    matchesType (IntVal _) IntType = True
    matchesType (StringVal _) StringType = True
    matchesType NilVal _ = True
    matchesType _ _ = False

prop_addColPreservesExistingData :: Database -> String -> [(String, ValType)] -> Property
prop_addColPreservesExistingData db tableName newCols =
  forAll (tableNameGen db) $
    \tableName ->
      let Just oldTable = Map.lookup tableName (fromDatabase db)
          oldRecords = recordsFromTable oldTable
          (_, DB db') = runState (add newCols tableName) db
          newRecords = recordsFromTable (fromJust $ Map.lookup tableName db')
       in all (\(oldRec, newRec) -> all (\col -> Map.lookup col (fromRecord oldRec) == Map.lookup col (fromRecord newRec)) (Map.keys $ fromRecord oldRec)) (zip oldRecords newRecords)

prop_renameTablePreservesRecords :: Database -> String -> Property
prop_renameTablePreservesRecords db newName =
  not (Map.member newName (fromDatabase db))
    ==> forAll (tableNameGen db)
    $ \oldName ->
      let Just oldTable = Map.lookup oldName (fromDatabase db)
          oldRecords = recordsFromTable oldTable
          (_, DB db') = runState (renameTable oldName newName) db
          newRecords = recordsFromTable (fromJust $ Map.lookup newName db')
       in newRecords === oldRecords

prop_SQLObjRoundTrip :: (SQLObj, [(TableName, SelectObj)]) -> Bool
prop_SQLObjRoundTrip (sqlobj, l) =
  let input = (sqlobj, map (\(TableName t, SelectObj so) -> (t, so)) l)
      printed = prettyPrintSQLObjWithCTEs input
      parsed = mainParse printed
   in case parsed of
        Left err -> False
        Right output -> input == output

runAllTests :: IO ()
runAllTests = do
  quickCheck prop_insertIncreasesSize
  quickCheck prop_createTableSuccess
  quickCheck prop_insertIntoNonExistingTable
  quickCheck prop_deleteFromNonExistingTable
  quickCheck prop_updateNonExistingTable
  quickCheck prop_createTableIncreasesDbSize
  quickCheck prop_deletedRecordsAbsent
  quickCheck prop_selectAllColumns
  quickCheck prop_insertionIntegrity
  quickCheck prop_insertionIncreasesCount
  quickCheck prop_updateAllRecords
  quickCheck prop_deleteEntireTableEmpty
  quickCheck prop_dropTableReducesDbSize
  quickCheck prop_addColumnIncreasesSchemaSize
  quickCheck prop_renameTableUpdatesName
  quickCheck prop_updateInvalidRecordFails
  quickCheck prop_createDuplicateTableFails
  quickCheck prop_deleteAllMakesTableEmpty
  quickCheck prop_addColumnPreservesRecordCount
  quickCheck prop_renameColumnUpdatesSchema
  quickCheck prop_insertInvalidTypeFails
  quickCheck prop_addColPreservesExistingData
  quickCheck prop_renameTablePreservesRecords
