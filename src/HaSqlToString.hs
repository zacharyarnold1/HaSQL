module HaSqlToString where

import Data.Char
import Data.List
import Data.Map qualified as Map
import HaSqlSyntax

-- converts value to string representation
valueToString :: Value -> String
valueToString NilVal = "nil"
valueToString (IntVal i) = show i
valueToString (StringVal s) = "'" ++ s ++ "'"
valueToString (ColVal c) = c

-- converts type of value to string representation
valTypeToString :: ValType -> String
valTypeToString IntType = "int"
valTypeToString StringType = "str"

-- converts table to string representation
tableToString :: Table -> String
tableToString (Table cols records) = dropWhileEnd isSpace $ unlines $ header : map showRecord records
  where
    colNames = Map.toList $ fromCols cols
    header = unwords $ map (\(name, t) -> name ++ " (" ++ valTypeToString t ++ ") |") colNames
    showRecord :: Record -> String
    showRecord record = unwords $ map (\(name, _) -> maybe "nil" valueToString (Map.lookup name $ fromRecord record) ++ " |") colNames

-- converts database to string representation
databaseToString :: Database -> String
databaseToString db = concatMap showTable $ Map.toList $ fromDatabase db
  where
    showTable :: (Name, Table) -> String
    showTable (name, table) = "{" ++ name ++ "}\n" ++ tableToString table ++ "\n{END_TABLE}\n"