import Control.Monad.State as S
import Data.Map qualified as Map
import HaSqlDB
import HaSqlSyntax
import Test.HUnit
import HaSqlDBOps
import HaSqlDBRecordOps

createTestDatabase :: Database
createTestDatabase = Map.empty

testStateA :: S.State Database String
testStateA = do
  _ <- create "testTable" [("id", IntType), ("name", StringType), ("grade", IntType)]
  _ <- insert "testTable" [("id", IntVal 1), ("name", StringVal "Rick Test"), ("grade", IntVal 3)]
  _ <- insert "testTable" [("id", IntVal 2), ("name", StringVal "Jack Test"), ("grade", IntVal 3)]
  _ <- insert "testTable" [("id", IntVal 3), ("name", StringVal "John Test")]
  _ <- insert "testTable" [("id", IntVal 4), ("name", StringVal "Zach Test"), ("grade", IntVal 3)]
  _ <- insert "testTable" [("id", IntVal 5), ("name", StringVal "Kailash Test"), ("grade", IntVal 3)]
  _ <- insert "testTable" [("id", IntVal 6), ("name", StringVal "Brian Test")]
  _ <- insert "testTable" [("id", IntVal 7), ("name", StringVal "Tom Test"), ("grade", IntVal 3)]
  _ <- insert "testTable" [("id", IntVal 8), ("name", StringVal "Peter Test"), ("grade", IntVal 3)]
  _ <- insert "testTable" [("id", IntVal 9), ("name", StringVal "Edward Test"), ("grade", IntVal 3)]
  deleteResult <- eval (DELETE "testTable" (OR (Clause (ColVal "id") (IntVal 2) EQS) (Clause (ColVal "id") (IntVal 5) EQS)))
  eval (SELECT Star "testTable" NONE)

testStateB :: S.State Database String
testStateB = do
  _ <- create "testTable" [("id", IntType), ("name", StringType)]
  _ <- insert "testTable" [("id", IntVal 1), ("name", StringVal "Alice")]
  _ <- insert "testTable" [("id", IntVal 2), ("name", StringVal "Bob")]
  eval (SELECT Star "testTable" NONE)

testStateC :: S.State Database String
testStateC = do
  _ <- create "testTable" [("id", IntType), ("name", StringType)]
  _ <- insert "testTable" [("id", IntVal 1), ("name", StringVal "Alice")]
  _ <- update "testTable" [("name", StringVal "Alice Updated")] (Clause (ColVal "id") (IntVal 1) EQS)
  eval (SELECT Star "testTable" NONE)

testStateD :: S.State Database String
testStateD = do
  _ <- create "testTable" [("id", IntType), ("name", StringType)]
  _ <- insert "testTable" [("id", IntVal 1), ("name", StringVal "Alice")]
  _ <- insert "testTable" [("id", IntVal 2), ("name", StringVal "Bob")]
  _ <- delete "testTable" (Clause (ColVal "id") (IntVal 1) EQS)
  eval (SELECT Star "testTable" NONE)

testStateE :: S.State Database String
testStateE = do
  _ <- create "testTable" [("id", IntType), ("name", StringType)]
  _ <- insert "testTable" [("id", IntVal 1), ("name", StringVal "Alice")]
  _ <- insert "testTable" [("id", IntVal 2), ("name", StringVal "Bob")]
  _ <- update "testTable" [("name", StringVal "Name Updated")] (OR (Clause (ColVal "id") (IntVal 1) EQS) (Clause (ColVal "id") (IntVal 2) EQS))
  eval (SELECT Star "testTable" NONE)

testStateF :: S.State Database String
testStateF = do
  _ <- create "testTable" [("id", IntType), ("name", StringType)]
  _ <- insert "testTable" [("id", IntVal 1), ("name", StringVal "Alice")]
  _ <- insert "testTable" [("id", IntVal 2), ("name", StringVal "Bob")]
  _ <- delete "testTable" NONE
  eval (SELECT Star "testTable" NONE)

testStateG :: S.State Database String
testStateG = do
  _ <- create "testTable" [("id", IntType), ("name", StringType), ("age", IntType)]
  _ <- insert "testTable" [("id", IntVal 1), ("name", StringVal "Alice"), ("age", IntVal 30)]
  _ <- insert "testTable" [("id", IntVal 2), ("name", StringVal "Bob")]
  eval (SELECT Star "testTable" NONE)

testStateH :: S.State Database String
testStateH = do
  _ <- create "testTable" [("id", IntType), ("name", StringType)]
  _ <- insert "testTable" [("id", IntVal 1), ("name", StringVal "Alice")]
  _ <- insert "testTable" [("id", IntVal 2), ("name", StringVal "Bob")]
  _ <- delete "testTable" (OR (Clause (ColVal "id") (IntVal 1) EQS) (Clause (ColVal "id") (IntVal 2) EQS))
  eval (SELECT Star "testTable" NONE)

testStateI :: S.State Database String
testStateI = do
  _ <- create "testTable" [("id", IntType), ("name", StringType)]
  _ <- insert "testTable" [("id", IntVal 1), ("name", StringVal "Alice")]
  _ <- insert "testTable" [("id", IntVal 2), ("name", StringVal "Bob")]
  eval (SELECT Star "testTable" (Clause (ColVal "id") (IntVal 1) EQS))

testStateJ :: S.State Database String
testStateJ = do
  _ <- create "testTable" [("id", IntType), ("name", StringType), ("age", IntType)]
  _ <- insert "testTable" [("id", IntVal 1), ("name", StringVal "Alice"), ("age", IntVal 30)]
  _ <- insert "testTable" [("id", IntVal 2), ("name", StringVal "Bob"), ("age", IntVal 25)]
  _ <- update "testTable" [("name", StringVal "Updated Name")] (AND (Clause (ColVal "age") (IntVal 25) GEQ) (Clause (ColVal "id") (IntVal 2) EQS))
  eval (SELECT Star "testTable" NONE)

testDatabaseOperationsA :: Test
testDatabaseOperationsA = TestCase $ do
  let (result, _) = runState testStateA createTestDatabase
  let expected = "grade (int) | id (int) | name (str) |\n3 | 9 | 'Edward Test' |\n3 | 8 | 'Peter Test' |\n3 | 7 | 'Tom Test' |\nnil | 6 | 'Brian Test' |\n3 | 4 | 'Zach Test' |\nnil | 3 | 'John Test' |\n3 | 1 | 'Rick Test' |\n"
  assertEqual "Test database operations A" expected result

testDatabaseOperationsB :: Test
testDatabaseOperationsB = TestCase $ do
  let (result, _) = runState testStateB createTestDatabase
  let expected = "id (int) | name (str) |\n2 | 'Bob' |\n1 | 'Alice' |\n"
  assertEqual "Test database operations B" expected result

testDatabaseOperationsC :: Test
testDatabaseOperationsC = TestCase $ do
  let (result, _) = runState testStateC createTestDatabase
  let expected = "id (int) | name (str) |\n1 | 'Alice Updated' |\n"
  assertEqual "Test database operations C" expected result

testDatabaseOperationsD :: Test
testDatabaseOperationsD = TestCase $ do
  let (result, _) = runState testStateD createTestDatabase
  let expected = "id (int) | name (str) |\n2 | 'Bob' |\n"
  assertEqual "Test database operations D" expected result

testDatabaseOperationsE :: Test
testDatabaseOperationsE = TestCase $ do
  let (result, _) = runState testStateE createTestDatabase
  let expected = "id (int) | name (str) |\n2 | 'Name Updated' |\n1 | 'Name Updated' |\n"
  assertEqual "Test database operations E" expected result

testDatabaseOperationsF :: Test
testDatabaseOperationsF = TestCase $ do
  let (result, _) = runState testStateF createTestDatabase
  let expected = "id (int) | name (str) |\n"
  assertEqual "Test database operations F" expected result

testDatabaseOperationsG :: Test
testDatabaseOperationsG = TestCase $ do
  let (result, _) = runState testStateG createTestDatabase
  let expected = "age (int) | id (int) | name (str) |\nnil | 2 | 'Bob' |\n30 | 1 | 'Alice' |\n"
  assertEqual "Test database operations G" expected result

testDatabaseOperationsH :: Test
testDatabaseOperationsH = TestCase $ do
  let (result, _) = runState testStateH createTestDatabase
  let expected = "id (int) | name (str) |\n"
  assertEqual "Test database operations H" expected result

testDatabaseOperationsI :: Test
testDatabaseOperationsI = TestCase $ do
  let (result, _) = runState testStateI createTestDatabase
  let expected = "id (int) | name (str) |\n1 | 'Alice' |\n"
  assertEqual "Test database operations I" expected result

testDatabaseOperationsJ :: Test
testDatabaseOperationsJ = TestCase $ do
  let (result, _) = runState testStateJ createTestDatabase
  let expected = "age (int) | id (int) | name (str) |\n25 | 2 | 'Updated Name' |\n30 | 1 | 'Alice' |\n"
  assertEqual "Test database operations J" expected result

runDBTests :: IO Counts
runDBTests = runTestTT $ TestList [testDatabaseOperationsA, testDatabaseOperationsB, testDatabaseOperationsC, testDatabaseOperationsD, testDatabaseOperationsE, testDatabaseOperationsF, testDatabaseOperationsG, testDatabaseOperationsH, testDatabaseOperationsI, testDatabaseOperationsJ]