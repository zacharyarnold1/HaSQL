module HaSqlUnitTests where

import Data.Map (fromList)
import HaSqlSyntax
import HaSqlTables
import Test.HUnit

-- Sample data for testing
table1 :: Table
table1 =
  Table
    (Cols $ fromList [("id", IntType), ("name", StringType)])
    [ Rec $ fromList [("id", IntVal 1), ("name", StringVal "Alice")],
      Rec $ fromList [("id", IntVal 2), ("name", StringVal "Bob")]
    ]

table2 :: Table
table2 =
  Table
    (Cols $ fromList [("userId", IntType), ("age", IntType)])
    [ Rec $ fromList [("userId", IntVal 1), ("age", IntVal 30)],
      Rec $ fromList [("userId", IntVal 3), ("age", IntVal 25)]
    ]

-- Test for INNER JOIN
testInnerJoin :: Test
testInnerJoin = TestCase $ do
  let result = innerJoin [("id", "userId")] table1 table2
  let expected =
        Table
          (Cols $ fromList [("id", IntType), ("name", StringType), ("userId", IntType), ("age", IntType)])
          [Rec $ fromList [("id", IntVal 1), ("name", StringVal "Alice"), ("userId", IntVal 1), ("age", IntVal 30)]]
  assertEqual "INNER JOIN should combine rows with matching ids" expected result

-- Test for LEFT JOIN
testLeftJoin :: Test
testLeftJoin = TestCase $ do
  let result = leftJoin [("id", "userId")] table1 table2
  let expected =
        Table
          (Cols $ fromList [("id", IntType), ("name", StringType), ("userId", IntType), ("age", IntType)])
          [ Rec $ fromList [("id", IntVal 1), ("name", StringVal "Alice"), ("userId", IntVal 1), ("age", IntVal 30)],
            Rec $ fromList [("id", IntVal 2), ("name", StringVal "Bob"), ("userId", NilVal), ("age", NilVal)]
          ]
  assertEqual "LEFT JOIN should include all rows from the left table" expected result

-- Test List
tests :: Test
tests =
  TestList
    [ TestLabel "Test Inner Join" testInnerJoin,
      TestLabel "Test Left Join" testLeftJoin
    ]

runJoinTests :: IO Counts

runJointests = runTestTT tests
