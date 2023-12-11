import Control.Monad.State as S
import qualified Data.Map as Map
import HaSqlDB
import HaSqlSyntax
import Test.HUnit

createTestDatabase :: Database
createTestDatabase = Map.fromList []

testState :: S.State Database String
testState = do
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
  selectResult <- eval (SELECT Star "testTable" NONE)
  return $ deleteResult ++ selectResult
