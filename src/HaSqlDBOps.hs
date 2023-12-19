module HaSqlDBOps where

import Control.Monad.State
import Data.List (intercalate, sortBy)
import Data.Map (Map)
import Data.Map qualified
import Data.Maybe
import HaSqlClauseLogic
import HaSqlSyntax
import HaSqlTables
import HaSqlToString

-- creates a table
create :: String -> [(String, ValType)] -> State Database (Maybe String)
create tableName columns = state $ \(DB db) ->
  if Data.Map.member tableName db
    then (Nothing, DB db) -- return a message that this table already exists
    else
      let newTable = Table (Cols (Data.Map.fromList columns)) []
       in (Just tableName, DB $ Data.Map.insert tableName newTable db)

-- deletes a table of specified name from the database
drop :: String -> State Database (Maybe String)
drop tableName = state $ \(DB db) ->
  if Data.Map.member tableName db
    then (Just tableName, DB $ Data.Map.delete tableName db)
    else (Nothing, DB db)

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

-- rename a table to specified name
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

-- update name of column in an individual record
renameRecord :: String -> String -> Record -> Record
renameRecord oldName newName (Rec record) =
  let maybeVal = Data.Map.lookup oldName record
      recordWithoutOld = Data.Map.delete oldName record
   in Rec $ maybe recordWithoutOld (\val -> Data.Map.insert newName val recordWithoutOld) maybeVal

-- View function,
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
        ++ intercalate "\n" (map showCol (Data.Map.toList cols))
    showCol :: (String, ValType) -> String
    showCol (colName, colType) = colName ++ " - " ++ valTypeToString colType
