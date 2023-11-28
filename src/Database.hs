module Database where

import CommandLine

data Database = 
    DB {
        dbName :: String,
        tables :: [Table]
    } 

data Table = Table {
    tableName :: String,
    columns :: [ColumnObj]
}