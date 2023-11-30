module CommandLine where

import Control.Applicative (Alternative (..))
import Parser

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
  | NOT Clause
  | NONE

reserved :: Char -> Bool
reserved c = c `elem` ['[', ']']

text :: Parser String
text = some (satisfy (not . reserved))

parseInput :: Parser SQLObj
parseInput = parseSelect <|> parseCreate <|> parseInsert <|> parseUpdate <|> parseDelete

parseSelect :: Parser SQLObj
parseSelect = SELECT <$> parseColumn <*> text <*> parseClause

parseCreate :: Parser SQLObj
parseCreate = CREATE <$> text <*> parseEntry

parseInsert :: Parser SQLObj
parseInsert = INSERT <$> text <*> parseEntry

parseUpdate :: Parser SQLObj
parseUpdate = UPDATE <$> text <*> parseEntry <*> parseClause

parseDelete :: Parser SQLObj
parseDelete = DELETE <$> text <*> parseClause

parseClause :: Parser Clause
parseClause = undefined

parseColumn :: Parser ColumnObj
parseColumn = undefined

parseEntry :: Parser [(String, Value)]
parseEntry = undefined

cli :: IO ()
cli = undefined

data Stepper = Stepper {
  tableName :: Maybe String
  data
}
