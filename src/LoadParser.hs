module LoadParser where

import Data.Char (isDigit)
import Data.Map (Map)
import Data.Map qualified as Map
import HaSqlSyntax
import Text.Parsec
import Text.Parsec.String

valTypeParser :: Parser ValType
valTypeParser =
  (string "int" >> return IntType)
    <|> (string "str" >> return StringType)

tupleParser :: Parser (String, ValType)
tupleParser = do
  name <- many1 (noneOf "0123456789 (){}[],\n'")
  spaces
  char '('
  vType <- valTypeParser
  char ')'
  return (name, vType)

headerParser :: Parser [(String, ValType)]
headerParser = tupleParser `endBy` (string " |" >> optional (char ' '))

stringLiteralParser :: Parser String
stringLiteralParser = do
  char '\''
  str <- many (noneOf "'")
  char '\''
  return str

valueParser :: Parser Value
valueParser =
  try (string "nil" >> return NilVal)
    <|> try (IntVal . read <$> many1 digit)
    <|> (stringLiteralParser >>= return . StringVal)

tableParser :: Parser (String, Table)
tableParser = do
  char '{'
  name <- many1 (noneOf "0123456789 (){}[],\n'")
  string "}\n"
  headers <- headerParser
  newline
  rows <- manyTill (recordParser headers) (try (string "{END_TABLE}"))
  return (name, Table (Cols $ Map.fromList headers) rows)

databaseParser :: Parser Database
databaseParser = do
  tables <- tableParser `endBy` spaces
  return $ DB $ Map.fromList tables

parseDatabaseString :: String -> Either ParseError Database
parseDatabaseString input = parse databaseParser "" input

recordParser :: [(String, ValType)] -> Parser Record
recordParser headers = Rec . Map.fromList <$> sequence [cellParser name | (name, _) <- headers]
  where
    cellParser name = do
      value <- valueParser
      optional (try (spaces >> char '|' >> spaces))
      return (name, value)

testParseHeader :: Either ParseError [(String, ValType)]
testParseHeader = parse headerParser "" "boss (str) | division (str) | name (str) | salary (int) |\n"

testParseTable :: Either ParseError (String, Table)
testParseTable = parse tableParser "" "{employeesA}\nboss (str) | division (str) | name (str) | salary (int) |\nnil | nil | 'Professor Weirich' | 5000000000 |\nnil | 'Finance' | 'Eli' | 100000 |\n'Bosswoman' | 'Biology' | 'Amanda' | 130000 |\n'Bosswoman' | 'HWE' | 'Shyam' | 150000 |\n'Bossman' | 'Swagonometry' | 'Zach' | nil |\n'Bossman' | 'SWE' | 'Kailash' | 100000 |\n{END_TABLE}"

testParseDatabase :: Either ParseError Database
testParseDatabase = parse databaseParser "" "{employeesA}\nboss (str) | division (str) | name (str) | salary (int) |\n'Professor Weirich' | 'Professor Weirich' | 'Professor Weirich' | 5000000000 |\nnil | 'Finance' | 'Eli' | 100000 |\n'Bosswoman' | 'Biology' | 'Amanda' | 130000 |\n'Bosswoman' | 'HWE' | 'Shyam' | 150000 |\n'Bossman' | 'Swagonometry' | 'Zach' | nil |\n'Bossman' | 'SWE' | 'Kailash' | 100000 |\n{END_TABLE}\n{names}\nage (int) | id (int) | name (str) |\n60 | 8 | 'Chip' |\n21 | 7 | 'Amanda' |\n25 | 6 | 'Shyam' |\n27 | 5 | 'Professor Weirich' |\nnil | 4 | 'Irene' |\n22 | 3 | 'Cassia' |\n23 | 2 | 'Zach' |\n21 | 1 | 'Kailash' |\n{END_TABLE}"