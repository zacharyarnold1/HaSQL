module HaSqlSyntax where

import Control.Applicative (Alternative (..))
import Data.Bool (Bool (True))

data SQLObj
  = SELECT
      { selectColumns :: ColumnObj,
        fromTable :: String,
        whereClauses :: Clause
      }
  | CREATE
      { tableName :: String,
        header :: [(String, ValType)]
      }
  | INSERT
      { tableName :: String,
        record :: [(String, Value)]
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
  = Clause Value Value ClauseOp
  | OR Clause Clause
  | AND Clause Clause
  | NOT Clause
  | NONE

data ClauseOp
  = EQ
  | LE
  | LT
  | GT
  | GE
  | NE

data ValType
  = IntType
  | StringType

clauseEq :: Value -> Value -> Maybe Bool
clauseEq NilVal NilVal = Just True
clauseEq (IntVal x) (IntVal y) = Just (x == y)
clauseEq (StringVal x) (StringVal y) = Just (x == y)
clauseEq _ _ = Nothing

clauseNeq :: Value -> Value -> Maybe Bool
clauseNeq NilVal NilVal = Just False
clauseNeq (IntVal x) (IntVal y) = Just (x /= y)
clauseNeq (StringVal x) (StringVal y) = Just (x /= y)
clauseNeq _ _ = Nothing

clauseLe :: Value -> Value -> Maybe Bool
clauseLe NilVal NilVal = Just False
clauseLe (IntVal x) (IntVal y) = Just (x < y)
clauseLe (StringVal x) (StringVal y) = Just (x < y)
clauseLe _ _ = Nothing

clauseGe :: Value -> Value -> Maybe Bool
clauseGe NilVal NilVal = Just False
clauseGe (IntVal x) (IntVal y) = Just (x > y)
clauseGe (StringVal x) (StringVal y) = Just (x > y)
clauseGe _ _ = Nothing

clauseLeq :: Value -> Value -> Maybe Bool
clauseLeq NilVal NilVal = Just True
clauseLeq (IntVal x) (IntVal y) = Just (x <= y)
clauseLeq (StringVal x) (StringVal y) = Just (x <= y)
clauseLeq _ _ = Nothing

clauseGeq :: Value -> Value -> Maybe Bool
clauseGeq NilVal NilVal = Just True
clauseGeq (IntVal x) (IntVal y) = Just (x >= y)
clauseGeq (StringVal x) (StringVal y) = Just (x >= y)
clauseGeq _ _ = Nothing
