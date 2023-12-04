module HaSqlSyntax where

import Control.Applicative (Alternative (..))
import Parser

data SQLObj
  = SELECT
      { selectColumns :: ColumnObj,
        fromTable :: String,
        whereClauses :: Clause
      }
  | CREATE
      { tableName :: String,
        columns :: [(String, Value)]
      }
  | INSERT
      { tableName :: String,
        columns :: [(String, Value)]
      }
  | UPDATE
      { tableName :: String,
        setClause :: [(String, Value)],
        updateWhere :: Clause
      }
  | DELETE
      { deleteFrom :: String,
        deleteWhere :: Clause
      }

data Value
  = NilVal
  | IntVal Int
  | StringVal String
  | ColVal String

data ColumnObj
  = Star
  | Columns [String]

data Clause
  = Clause Value Value (Value -> Value -> Bool)
  | OR Clause Clause
  | AND Clause Clause
  | NOT Clause
  | NONE
