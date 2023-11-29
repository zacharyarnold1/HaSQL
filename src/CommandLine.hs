module CommandLine where

import Parser
import Control.Applicative (Alternative(..))

data SQLObj
  = SELECT
      { selectColumns :: ColumnObj,
        fromTable :: String,
        whereClauses :: Clause
      }
  | CREATE
      { tableName :: String,
        columns :: [(String, Value)]
      }
  | INSERT
      { tableName :: String,
        columns :: [(String, Value)]
      }
  | UPDATE
      { tableName :: String,
        setClause :: [(String, Value)],
        updateWhere :: Clause
      }
  | DELETE
      { deleteFrom :: String,
        deleteWhere :: Clause
      }

data Value
  = NilVal
  | IntVal Int
  | StringVal String
  | ColVal String

data ColumnObj
  = Star
  | Columns [String]

data Clause
  = Clause Value Value (Value -> Value -> Bool)
  | OR Clause Clause
  | AND Clause Clause
  | NONE


parseInput :: Parser SQLObj
parseInput = parseSelect <|> parseCreate <|> parseInsert <|> parseUpdate <|> parseDelete

parseSelect :: Parser SQLObj
parseSelect = undefined

parseCreate :: Parser SQLObj
parseCreate = undefined

parseInsert :: Parser SQLObj
parseInsert = undefined

parseUpdate :: Parser SQLObj
parseUpdate = undefined

parseDelete :: Parser SQLObj
parseDelete = undefined

parseClause :: Parser Clause 
parseClause = undefined

cli :: IO ()
cli = undefined
