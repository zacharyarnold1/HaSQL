module ParserTest where

import HaSqlMainParser
import HaSqlSyntax
import Test.HUnit
import Text.Parsec (parse)

-- Test for parseSelect
testParseSelect :: Test
testParseSelect = TestCase $ do
  let input = "[GET] (colA, colB) [IN] (myTable) [IF] ({colA == 'value1'})"
  let expected = Right (SELECT {selectColumns = Columns ["colA", "colB"], fromTable = TName "myTable", whereClauses = Clause (ColVal "colA") (StringVal "value1") EQS, order = ([], False)}, [])
  assertEqual "parse select statement" expected (mainParse input)

testParseSelectStar :: Test
testParseSelectStar = TestCase $ do
  let input = "[GET] (*) [IN] (myTable) [IF] ({colA == 'value1'})"
  let expected = Right (SELECT {selectColumns = Star, fromTable = TName "myTable", whereClauses = Clause (ColVal "colA") (StringVal "value1") EQS, order = ([], False)}, [])
  assertEqual "parse select statement" expected (mainParse input)

testParseSelectComplexClause :: Test
testParseSelectComplexClause = TestCase $ do
  let input = "[GET] (colA, colB) [IN] (myTable) [IF] ({{colA == 'value1'} AND {NOT {colB == 'value2'}}})"
  let expected = Right (SELECT {selectColumns = Columns ["colA", "colB"], fromTable = TName "myTable", whereClauses = AND (Clause (ColVal "colA") (StringVal "value1") EQS) (NOT (Clause (ColVal "colB") (StringVal "value2") EQS)), order = ([], False)}, [])
  assertEqual "parse select statement" expected (mainParse input)

-- Test for parseCreate
testParseCreate :: Test
testParseCreate = TestCase $ do
  let input = "[MAKE] (myTable) [WITH] ({colA, int}, {colB, string})"
  let expected = Right (CREATE {tableName = "myTable", header = [("colA", IntType), ("colB", StringType)]}, [])
  assertEqual "parse create statement" expected (mainParse input)

-- Test for parseInsert
testParseInsert :: Test
testParseInsert = TestCase $ do
  let input = "[PUT] (myTable) [WITH] ({colA,'value1'}, {colB, 10})"
  let expected = Right (INSERT {tableName = "myTable", record = [("colA", StringVal "value1"), ("colB", IntVal 10)]}, [])
  assertEqual "parse insert statement" expected (mainParse input)

-- Test for parseUpdate
testParseUpdate :: Test
testParseUpdate = TestCase $ do
  let input = "[CHANGE] (myTable) [TO] ({colA,'newValue'}) [IF] ({colB > 5})"
  let expected = Right (UPDATE {tableName = "myTable", record = [("colA", StringVal "newValue")], updateWhere = Clause (ColVal "colB") (IntVal 5) GTH}, [])
  assertEqual "parse update statement" expected (mainParse input)

-- Test for parseDelete
testParseDelete :: Test
testParseDelete = TestCase $ do
  let input = "[REMOVEFROM] (myTable) [IF] ({col != 'value1'})"
  let expected = Right (DELETE {deleteFrom = "myTable", deleteWhere = Clause (ColVal "col") (StringVal "value1") NEQ}, [])
  assertEqual "parse delete statement" expected (mainParse input)

-- Run the tests
runParserTests :: IO Counts
runParserTests = runTestTT $ TestList [testParseSelect, testParseSelectStar, testParseSelectComplexClause, testParseCreate, testParseInsert, testParseUpdate, testParseDelete]