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
        record :: [(String, Value)],
        updateWhere :: Clause
      }
  | DELETE
      { deleteFrom :: String,
        deleteWhere :: Clause
      }
  deriving (Show)

instance Eq SQLObj where
  (==) (SELECT co s cl) (SELECT co2 s2 cl2) = (co == co2) && (s == s2) && (cl == cl2)
  (==) (CREATE s h) (CREATE s2 h2) = (s == s2) && (h == h2)
  (==) (INSERT s r) (INSERT s2 r2) = (s == s2) && (r == r2)
  (==) (UPDATE s r cl) (UPDATE s2 r2 cl2) = (s == s2) && (r == r2) && (cl == cl2)
  (==) (DELETE s cl) (DELETE s2 cl2) = (s == s2) && (cl == cl2)
  (==) _ _ = False

data Value
  = NilVal
  | IntVal Int
  | StringVal String
  | ColVal String
  deriving (Show)

instance Eq Value where
  (==) NilVal NilVal = True
  (==) (IntVal i) (IntVal i2) = i == i2
  (==) (StringVal s) (StringVal s2) = s == s2
  (==) (ColVal c) (StringVal c2) = c == c2
  (==) _ _ = False

data ColumnObj
  = Star
  | Columns [String]
  deriving (Show)

instance Eq ColumnObj where
  (==) Star Star = True
  (==) (Columns cs) (Columns cs2) = cs == cs2
  (==) _ _ = False

data Clause
  = Clause Value Value ClauseOp
  | OR Clause Clause
  | AND Clause Clause
  | NOT Clause
  | NONE
  deriving (Show)

instance Eq Clause where
  (==) (Clause v1 v2 co) (Clause v3 v4 co2) = (v1 == v3) && (v2 == v4) && (co == co2)
  (==) (OR c1 c2) (OR c3 c4) = (c1 == c3) && (c2 == c4)
  (==) (AND c1 c2) (AND c3 c4) = (c1 == c3) && (c2 == c4)
  (==) (NOT c1) (NOT c2) = c1 == c2
  (==) NONE NONE = True
  (==) _ _ = False

data ClauseOp
  = EQS
  | LEQ
  | LTH
  | GTH
  | GEQ
  | NEQ
  deriving (Show)

instance Eq ClauseOp where
  (==) EQS EQS = True
  (==) LEQ LEQ = True
  (==) LTH LTH = True
  (==) GTH GTH = True
  (==) GEQ GEQ = True
  (==) NEQ NEQ = True
  (==) _ _ = False

data ValType
  = IntType
  | StringType
  deriving (Show)

instance Eq ValType where
  (==) IntType IntType = True
  (==) StringType StringType = True
  (==) _ _ = False

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
