module HaSqlDB where

import Control.Monad.State
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Map (Map)
import Data.Map qualified
import Data.Maybe
import Data.Ord (Down (Down), comparing)
import HaSqlClauseLogic
import HaSqlDBOps
import HaSqlDBRecordOps
import HaSqlDBSelect
import HaSqlMainParser
import HaSqlSyntax
import HaSqlTables
import HaSqlToString

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
            result <- HaSqlDBOps.drop tableName
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

-- Helper for script excuter, executes a single line of the script
executeLine :: Database -> String -> IO Database
executeLine db line = case mainParse line of
  Left err -> print err >> return db
  Right sql ->
    let (s, a) = runState (eval sql) db
     in return a

-- Runs a script of DSL commands, repeatedly calls executeLine
runScript :: Database -> FilePath -> IO Database
runScript db filePath = do
  content <- readFile ("scripts/" ++ filePath)
  let linesOfContent = lines content
  db2 <- foldM executeLine db linesOfContent
  putStrLn "Script executed"
  return db2

-- For a inputted script, returns if that script is valid or not
validateScript :: FilePath -> Bool
validateScript = undefined