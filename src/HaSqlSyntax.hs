module HaSqlSyntax where

import Control.Applicative (Alternative (..))
import Control.Monad
import Data.Bits (Bits (xor))
import Data.Bool (Bool (True))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Map (Map)
import Data.Map qualified as Map
import Test.QuickCheck

data SQLObj
  = SELECT
      { selectColumns :: ColumnObj,
        fromTable :: TableObj,
        whereClauses :: Clause,
        grouping :: [String],
        order :: ([String], Bool)
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
  deriving (Show, Eq)

data Value
  = NilVal
  | IntVal Int
  | StringVal String
  | ColVal String
  deriving (Show, Eq)

data ColumnObj
  = Star
  | Columns [String]
  deriving (Show, Eq)

data TableObj
  = TName String
  | CTE SQLObj
  | INNERJOIN TableObj TableObj [(String, String)]
  | LEFTJOIN TableObj TableObj [(String, String)]
  | RIGHTJOIN TableObj TableObj [(String, String)]
  | FULLJOIN TableObj TableObj [(String, String)]
  | NATURALJOIN TableObj TableObj
  deriving (Show, Eq)

data Clause
  = Clause Value Value ClauseOp
  | OR Clause Clause
  | AND Clause Clause
  | NOT Clause
  | NONE
  deriving (Show, Eq)

data ClauseOp
  = EQS
  | LEQ
  | LTH
  | GTH
  | GEQ
  | NEQ
  deriving (Show, Eq)

data ValType
  = IntType
  | StringType
  deriving (Show, Eq)

data Command
  = SAVE
  | SAVEAS String
  | LOAD String
  | NEW String
  | QUIT
  | SCRIPT String
  deriving (Show, Eq)

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

data DBLoad = DBLoad (Maybe Database) (Maybe String)

newtype Cols = Cols (Map String ValType)
  deriving (Show)

type Name = String

newtype Database = DB (Map Name Table)

data Table = Table Cols [Record]
  deriving (Show)

newtype Record = Rec (Map String Value)
  deriving (Show)

fromRecord :: Record -> Map String Value
fromRecord (Rec x) = x

fromDatabase :: Database -> Map Name Table
fromDatabase (DB x) = x

fromCols :: Cols -> Map String ValType
fromCols (Cols x) = x

valueToString :: Value -> String
valueToString NilVal = "nil"
valueToString (IntVal i) = show i
valueToString (StringVal s) = "'" ++ s ++ "'"
valueToString (ColVal c) = c

valTypeToString :: ValType -> String
valTypeToString IntType = "int"
valTypeToString StringType = "str"

tableToString :: Table -> String
tableToString (Table cols records) = dropWhileEnd isSpace $ unlines $ header : map showRecord records
  where
    colNames = Map.toList $ fromCols cols
    header = unwords $ map (\(name, t) -> name ++ " (" ++ valTypeToString t ++ ") |") colNames
    showRecord :: Record -> String
    showRecord record = unwords $ map (\(name, _) -> maybe "nil" valueToString (Map.lookup name $ fromRecord record) ++ " |") colNames

databaseToString :: Database -> String
databaseToString db = concatMap showTable $ Map.toList $ fromDatabase db
  where
    showTable :: (Name, Table) -> String
    showTable (name, table) = "{" ++ name ++ "}\n" ++ tableToString table ++ "\n{END_TABLE}\n"

instance Arbitrary ValType where
  arbitrary = elements [IntType, StringType]

instance Arbitrary Value where
  arbitrary =
    oneof
      [ return NilVal,
        IntVal <$> arbitrary,
        StringVal <$> arbitrary,
        ColVal <$> arbitrary
      ]

arbitraryColumnName :: Gen String
arbitraryColumnName = listOf1 $ elements ['a' .. 'z']

arbitraryStringValue :: Gen String
arbitraryStringValue = listOf1 $ suchThat arbitrary (/= '\'')

arbitraryValue :: ValType -> Gen Value
arbitraryValue IntType =
  frequency
    [ (1, return NilVal),
      (3, IntVal <$> arbitrary)
    ]
arbitraryValue StringType =
  frequency
    [ (1, return NilVal),
      (3, StringVal <$> arbitraryStringValue)
    ]

instance Arbitrary Cols where
  arbitrary = do
    keys <- listOf1 arbitraryColumnName
    vals <- vectorOf (length keys) arbitrary
    return $ Cols $ Map.fromList (zip keys vals)

arbitraryRecord :: Cols -> Gen Record
arbitraryRecord cs = Rec . Map.fromList <$> mapM createEntry (Map.toList $ fromCols cs)
  where
    createEntry :: (String, ValType) -> Gen (String, Value)
    createEntry (name, vType) = do
      value <- arbitraryValue vType
      return (name, value)

instance Arbitrary Table where
  arbitrary = do
    cols <- arbitrary
    records <- listOf (arbitraryRecord cols)
    return $ Table cols records

instance Arbitrary Database where
  arbitrary = do
    keys <- listOf1 arbitraryColumnName
    vals <- vectorOf (length keys) arbitrary
    return $ DB $ Map.fromList (zip keys vals)

instance Arbitrary ClauseOp where
  arbitrary = elements [EQS, LEQ, LTH, GTH, GEQ, NEQ]

instance Arbitrary Clause where
  arbitrary = sized clauseGen

clauseGen :: Int -> Gen Clause
clauseGen 0 = return NONE -- Base case to stop recursion
clauseGen n =
  oneof
    [ liftM3 Clause arbitrary arbitrary arbitrary,
      liftM2 OR subClause subClause,
      liftM2 AND subClause subClause,
      NOT <$> subClause,
      return NONE
    ]
  where
    subClause = clauseGen (n `div` 2)