module HaSqlSyntax where

import Control.Applicative (Alternative (..))
import Control.Monad
import Data.Map (Map)

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

-- Wrapper to pair database with its name
data DBLoad = DBLoad (Maybe Database) (Maybe String)

-- Schema of a table (column names and their types)
newtype Cols = Cols (Map String ValType)
  deriving (Show, Eq)

type Name = String

-- Database, mapping table names to tables.
newtype Database = DB (Map Name Table)
  deriving (Show, Eq)

-- Table in a database (columns and records).
data Table = Table Cols [Record]
  deriving (Show, Eq)

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

-- returns the columns (attributes with column names and type) from an inputted table
colsFromTable :: Table -> Cols
colsFromTable (Table cols _) = cols

-- returns all the records from an inputted table
recordsFromTable :: Table -> [Record]
recordsFromTable (Table _ records) = records
