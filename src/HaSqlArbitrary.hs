module HaSqlArbitrary where

import Control.Monad
import Data.Map qualified as Map
import HaSqlSyntax
import Test.QuickCheck

-- Generates arbitrary ValType
instance Arbitrary ValType where
  arbitrary = elements [IntType, StringType]

-- Generates arbitrary Value values
instance Arbitrary Value where
  arbitrary =
    oneof
      [ return NilVal,
        IntVal <$> arbitrary,
        StringVal <$> arbitraryStringValue,
        ColVal <$> arbitraryColumnName
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

instance Arbitrary SQLObj where
  arbitrary = oneof [genSelect, genCreate, genInsert, genUpdate, genDelete, genDrop, genAdd, genRenameCol, genRenameTable]

genSelect :: Gen SQLObj
genSelect = do
  cols <- arbitrary
  table <- arbitrary
  whereClause <- arbitrary
  order <- arbitrary
  return $ SELECT cols table whereClause order

genCreate :: Gen SQLObj
genCreate = do
  tableName <- arbitraryColumnName
  header <- genHeaderPairs
  return $ CREATE tableName header

genInsert :: Gen SQLObj
genInsert = do
  tableName <- arbitraryColumnName
  record <- arbitrary
  return $ INSERT tableName record

genUpdate :: Gen SQLObj
genUpdate = do
  tableName <- arbitrary
  record <- arbitrary
  updateWhere <- arbitrary
  return $ UPDATE tableName record updateWhere

genDelete :: Gen SQLObj
genDelete = do
  deleteFrom <- arbitrary
  deleteWhere <- arbitrary
  return $ DELETE deleteFrom deleteWhere

genDrop :: Gen SQLObj
genDrop = do
  tableName <- arbitrary
  return $ DROP tableName

genAdd :: Gen SQLObj
genAdd = do
  tableName <- arbitrary
  newCols <- arbitrary
  return $ ADD tableName newCols

genRenameCol :: Gen SQLObj
genRenameCol = do
  tableName <- arbitrary
  oldColName <- arbitrary
  newColName <- arbitrary
  return $ RENAMECOL tableName oldColName newColName

genRenameTable :: Gen SQLObj
genRenameTable = do
  tableName <- arbitrary
  newTableName <- arbitrary
  return $ RENAMETABLE tableName newTableName

-- Arbitrary instance for ColumnObj
instance Arbitrary ColumnObj where
  arbitrary =
    oneof
      [ return Star,
        Columns <$> listOf genColName
      ]

-- Helper function to generate a column name
genColName :: Gen String
genColName = elements ["id", "name", "age", "userId", "address", "email"]

-- Arbitrary instance for a list of (String, ValType)
genHeaderPairs :: Gen [(String, ValType)]
genHeaderPairs = listOf $ (,) <$> genColName <*> arbitrary

-- Arbitrary instance for TableObj
instance Arbitrary TableObj where
  arbitrary = sized genTableObj

-- Add more constructors here if TableObj

genTableObj :: Int -> Gen TableObj
genTableObj i =
  if i <= 0
    then TName <$> genTableName
    else
      oneof
        [ TName <$> genTableName,
          INNERJOIN <$> genTableObj (i `div` 2) <*> genTableObj (i `div` 2) <*> genColNamePairs,
          LEFTJOIN <$> genTableObj (i `div` 2) <*> genTableObj (i `div` 2) <*> genColNamePairs,
          RIGHTJOIN <$> genTableObj (i `div` 2) <*> genTableObj (i `div` 2) <*> genColNamePairs,
          FULLJOIN <$> genTableObj (i `div` 2) <*> genTableObj (i `div` 2) <*> genColNamePairs,
          NATURALJOIN <$> genTableObj (i `div` 2) <*> genTableObj (i `div` 2)
        ]

-- Helper function to generate a table name
genTableName :: Gen String
genTableName = elements ["table1", "table2", "table3", "users", "orders"]

-- Helper function to generate a list of column name pairs
genColNamePairs :: Gen [(String, String)]
genColNamePairs = listOf $ (,) <$> genColName <*> genColName
