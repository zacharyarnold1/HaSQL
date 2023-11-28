module CommandLine where

import Parser

data SQLObj =
    SELECT {
      selectColumns :: ColumnObj,
      fromTable :: String,
      whereClause :: Maybe String
    }
  | CREATE {
      tableName :: String,
      columns :: [(String, String)]
    }
  | INSERT {
      tableName :: String,
      columns :: [(String, String)]
    }
  | UPDATE {
      tableName :: String,
      setClause :: [(String, String)],
      updateWhere :: Maybe String
    }
  | DELETE {
      deleteFrom :: String,
      deleteWhere :: Maybe String
    }

data ColumnObj = 
    Star
  | Columns [String]


parseInput :: Parser SQLObj = undefined




