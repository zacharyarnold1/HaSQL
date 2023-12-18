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

-- Represents a SQL operation, i.e. the CRUD operations
data SQLObj
  = SELECT
      { selectColumns :: ColumnObj,
        fromTable :: TableObj,
        whereClauses :: Clause,
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
  | DROP
      { tableName :: String
      }
  | ADD
      { tableName :: String,
        newCols :: [(String, ValType)]
      }
  | RENAMECOL
      { tableName :: String,
        oldColName :: String,
        newColName :: String
      }
  | RENAMETABLE
      { tableName :: String,
        newTableName :: String
      }
  | VIEW
  deriving (Show, Eq)

-- Values in clauses, records, and commands can be of type Nil, Int, String, or Column names
data Value
  = NilVal
  | IntVal Int
  | StringVal String
  | ColVal String
  deriving (Show, Eq)

-- how columns are selected in a GET statement
data ColumnObj
  = Star
  | Columns [String]
  deriving (Show, Eq)

-- Table or a table-like structure in SQL. Table can be nested with other TableObj's
data TableObj
  = TName String
  | CTE SQLObj
  | INNERJOIN TableObj TableObj [(String, String)]
  | LEFTJOIN TableObj TableObj [(String, String)]
  | RIGHTJOIN TableObj TableObj [(String, String)]
  | FULLJOIN TableObj TableObj [(String, String)]
  | NATURALJOIN TableObj TableObj
  deriving (Show, Eq)

-- A condition or a logical clause in SQL.
data Clause
  = Clause Value Value ClauseOp
  | OR Clause Clause
  | AND Clause Clause
  | NOT Clause
  | NONE
  deriving (Show, Eq)

-- A comparison operation in a clause.
data ClauseOp
  = EQS
  | LEQ
  | LTH
  | GTH
  | GEQ
  | NEQ
  deriving (Show, Eq)

-- The type of a value in a record of a table
data ValType
  = IntType
  | StringType
  deriving (Show, Eq)

-- A command in a database management context
data Command
  = SAVE
  | SAVEAS String
  | LOAD String
  | NEW String
  | QUIT
  | SCRIPT String
  | DELETEDB String
  deriving (Show, Eq)

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

-- Wrapper to pair database with its name
data DBLoad = DBLoad (Maybe Database) (Maybe String)

-- Schema of a table (column names and their types)
newtype Cols = Cols (Map String ValType)
  deriving (Show)

type Name = String

-- Database, mapping table names to tables.
newtype Database = DB (Map Name Table)
  deriving (Show)

-- Table in a database (columns and records).
data Table = Table Cols [Record]
  deriving (Show)

-- Record in a table, maps column names to values
newtype Record = Rec (Map String Value)
  deriving (Eq, Show)

-- extracts map of values from record
fromRecord :: Record -> Map String Value
fromRecord (Rec x) = x

-- extracts map of tables from database
fromDatabase :: Database -> Map Name Table
fromDatabase (DB x) = x

-- extract map of column types from Cols
fromCols :: Cols -> Map String ValType
fromCols (Cols x) = x

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

-- Generates arbitrary ValType
instance Arbitrary ValType where
  arbitrary = elements [IntType, StringType]

-- Generates arbitrary Value values
instance Arbitrary Value where
  arbitrary =
    oneof
      [ return NilVal,
        IntVal <$> arbitrary,
        StringVal <$> arbitrary,
        ColVal <$> arbitrary
      ]

-- Generates arbitrary column name
arbitraryColumnName :: Gen String
arbitraryColumnName = listOf1 $ elements ['a' .. 'z']

-- Generates arbitrary String values
arbitraryStringValue :: Gen String
arbitraryStringValue = listOf1 $ suchThat arbitrary (/= '\'')

-- Generates arbitrary value based on given ValType
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

-- Generates arbitry Cols (column schema)
instance Arbitrary Cols where
  arbitrary = do
    keys <- listOf1 arbitraryColumnName
    vals <- vectorOf (length keys) arbitrary
    return $ Cols $ Map.fromList (zip keys vals)

-- Generates a list of (column name, Value) pairs matching the column schema of a table
genMatchingRecord :: Table -> Gen [(String, Value)]
genMatchingRecord (Table cols _) = arbitraryKeyVal cols

-- Helper function to generate a list of (column name, Value) pairs for given Cols
arbitraryKeyVal :: Cols -> Gen [(String, Value)]
arbitraryKeyVal cs = mapM createEntry (Map.toList $ fromCols cs)
  where
    createEntry :: (String, ValType) -> Gen (String, Value)
    createEntry (name, vType) = do
      value <- arbitraryValue vType
      return (name, value)

-- Instance for generating arbitrary Records based on given columns
arbitraryRecord :: Cols -> Gen Record
arbitraryRecord cs = Rec . Map.fromList <$> arbitraryKeyVal cs

-- Generates arbitrary Table
instance Arbitrary Table where
  arbitrary = do
    cols <- arbitrary
    records <- listOf (arbitraryRecord cols)
    return $ Table cols records

-- Generates arbitrary Database
instance Arbitrary Database where
  arbitrary = do
    keys <- listOf1 arbitraryColumnName
    vals <- vectorOf (length keys) arbitrary
    return $ DB $ Map.fromList (zip keys vals)

-- Generates arbitrary cluase operator
instance Arbitrary ClauseOp where
  arbitrary = elements [EQS, LEQ, LTH, GTH, GEQ, NEQ]

-- Generates arbitrary clause
instance Arbitrary Clause where
  arbitrary = sized clauseGen

-- Recursive generator for arbitrary Clause values
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

-- Generator for arbitrary table names from a given Database
tableNameGen :: Database -> Gen String
tableNameGen (DB db) =
  if Map.null db
    then return "defaultTable" -- Fallback for an empty database
    else elements (Map.keys db)