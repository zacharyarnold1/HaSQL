module Database where

import CommandLine

data Database = 
    DB {
        dbName :: String,
        tables :: [Table]
    } 

data Table = Table {
    tableName :: String,
    column :: ColumnObj,
    entries :: [Record]
}

data Record = Record {
    
}