module HaSqlLoadParser where

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
parseDatabaseString = parse databaseParser ""

recordParser :: [(String, ValType)] -> Parser Record
recordParser headers = Rec . Map.fromList <$> sequence [cellParser name | (name, _) <- headers]
  where
    cellParser name = do
      value <- valueParser
      optional (try (spaces >> char '|' >> spaces))
      return (name, value)