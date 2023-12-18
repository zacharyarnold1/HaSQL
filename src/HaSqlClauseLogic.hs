module HaSqlClauseLogic where

import Data.Map qualified
import Data.Maybe
import HaSqlSyntax

-- compares two value instances for equality
clauseEq :: Value -> Value -> Maybe Bool
clauseEq NilVal NilVal = Just True
clauseEq (IntVal x) (IntVal y) = Just (x == y)
clauseEq (StringVal x) (StringVal y) = Just (x == y)
clauseEq _ _ = Nothing

-- compares two value instances for inequality
clauseNeq :: Value -> Value -> Maybe Bool
clauseNeq NilVal NilVal = Just False
clauseNeq (IntVal x) (IntVal y) = Just (x /= y)
clauseNeq (StringVal x) (StringVal y) = Just (x /= y)
clauseNeq _ _ = Nothing

-- compares two value instances to check if the first is less than the second
clauseLe :: Value -> Value -> Maybe Bool
clauseLe NilVal NilVal = Just False
clauseLe (IntVal x) (IntVal y) = Just (x < y)
clauseLe (StringVal x) (StringVal y) = Just (x < y)
clauseLe _ _ = Nothing

-- compares two value instances to check if the first is greater than the second
clauseGe :: Value -> Value -> Maybe Bool
clauseGe NilVal NilVal = Just False
clauseGe (IntVal x) (IntVal y) = Just (x > y)
clauseGe (StringVal x) (StringVal y) = Just (x > y)
clauseGe _ _ = Nothing

-- compares two value instances to check if the first is less or equal to the second
clauseLeq :: Value -> Value -> Maybe Bool
clauseLeq NilVal NilVal = Just True
clauseLeq (IntVal x) (IntVal y) = Just (x <= y)
clauseLeq (StringVal x) (StringVal y) = Just (x <= y)
clauseLeq _ _ = Nothing

-- compares two value instances to check if the first is greater than or equak to the second
clauseGeq :: Value -> Value -> Maybe Bool
clauseGeq NilVal NilVal = Just True
clauseGeq (IntVal x) (IntVal y) = Just (x >= y)
clauseGeq (StringVal x) (StringVal y) = Just (x >= y)
clauseGeq _ _ = Nothing

-- given a clause and record, returns wether or not the record matches the clause
evalClause :: Clause -> Record -> Bool
evalClause clause record = case clause of
  Clause val1 val2 op -> evalOp op (resolveValue val1) (resolveValue val2)
  AND c1 c2 -> evalClause c1 record && evalClause c2 record
  OR c1 c2 -> evalClause c1 record || evalClause c2 record
  NOT c -> not (evalClause c record)
  NONE -> True
  where
    resolveValue :: Value -> Value
    resolveValue v@(ColVal colName) = Data.Maybe.fromMaybe v (Data.Map.lookup colName (fromRecord record))
    resolveValue v = v
    evalOp :: ClauseOp -> Value -> Value -> Bool
    evalOp op v1 v2 = case op of
      EQS -> Data.Maybe.fromMaybe False (clauseEq v1 v2)
      NEQ -> Data.Maybe.fromMaybe False (clauseNeq v1 v2)
      LTH -> Data.Maybe.fromMaybe False (clauseLe v1 v2)
      GTH -> Data.Maybe.fromMaybe False (clauseGe v1 v2)
      LEQ -> Data.Maybe.fromMaybe False (clauseLeq v1 v2)
      GEQ -> Data.Maybe.fromMaybe False (clauseGeq v1 v2)