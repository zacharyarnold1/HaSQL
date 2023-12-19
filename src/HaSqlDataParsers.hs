module HaSqlDataParsers where

import HaSqlBasicParsers
import HaSqlSyntax
import Text.Parsec
import Text.Parsec.String (Parser)

-- parses column names
parseColumns :: Parser ColumnObj
parseColumns =
  try (string "(*)" >> return Star)
    <|> try (Columns <$> columnListParser)

-- parses list of column, value pairs, utilized in CHANGE (update) parser
parseRecord :: Parser [(String, Value)]
parseRecord = do
  char '('
  pairs <- sepBy keyValueParser (char ',' >> spaces)
  char ')'
  return pairs

-- parses list of column, value pairs, utilized in PUT (update) parser (ColVals not supported as values, only Nils, Ints, and Strings)
parseRecordNoCol :: Parser [(String, Value)]
parseRecordNoCol = do
  char '('
  pairs <- sepBy keyValueParserNoCol (char ',' >> spaces)
  char ')'
  return pairs

-- parses table name
parseTableName :: Parser TableObj
parseTableName = do
  name <- many1 (noneOf "0123456789 (){}[],\n'")
  return $ TName name

-- parses header (i.e. schema) of a table, string (column name), valType (column type) tuples
parseHeader :: Parser [(String, ValType)]
parseHeader = do
  char '('
  pairs <- sepBy keyTypeParser (char ',' >> spaces)
  char ')'
  return pairs
