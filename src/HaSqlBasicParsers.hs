module HaSqlBasicParsers where

import HaSqlSyntax
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as Token

-- Define a language lexer
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    ops = ["[GET]", "[IN]", "[IF]", "[MAKE]", "[PUT]", "[CHANGE]", "[TO]", "[WITH]", "[REMOVEFROM]", "[LET]", "[BE]", "[ORDER]", "[MATCH]", "[MATCHLEFT]", "[MATCHRIGHT]", "[MATCHFULL]", "[MATCHREG]", "[SUCHTHAT]", "[ADD COLS]", "[RENAME COL]", "[RENAME TABLE]", "[DELETE TABLE]", "[VIEW]"]
    names = []
    style =
      emptyDef
        { Token.reservedOpNames = ops,
          Token.reservedNames = names
        }

-- Define parsers for operators
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

-- Define a parser for identifiers (like table names, column names)
identifier :: Parser String
identifier = Token.identifier lexer

-- parses ValTypes (NilVal, IntVal, StringVal, ColVal)
valueParser :: Parser Value
valueParser =
  try (string "Nil" >> return NilVal)
    <|> try (intParser >>= return . IntVal)
    <|> try (stringLiteralParser >>= return . StringVal)
    <|> try (nameParser >>= return . ColVal)

-- same as valueParser, but does not support ColVals
valueParserNoCol :: Parser Value
valueParserNoCol =
  try (string "Nil" >> return NilVal)
    <|> try (intParser >>= return . IntVal)
    <|> try (stringLiteralParser >>= return . StringVal)

-- parses Integers
intParser :: Parser Int
intParser = do
  digits <- many1 digit
  return (read digits)

-- parses string literals
stringLiteralParser :: Parser String
stringLiteralParser = do
  char '\''
  str <- many (noneOf "'")
  char '\''
  return str

-- parses names
nameParser :: Parser String
nameParser = do
  many1 (noneOf "0123456789 (){}[],\n'")

-- parses column, value pairs, helper to parseRecord
keyValueParser :: Parser (String, Value)
keyValueParser = do
  char '{'
  key <- many1 (noneOf "0123456789 (){}[],\n'")
  char ','
  spaces
  value <- valueParser
  char '}'
  return (key, value)

-- parses column, value pairs, helper to parseRecordNoCol
keyValueParserNoCol :: Parser (String, Value)
keyValueParserNoCol = do
  char '{'
  key <- many1 (noneOf "0123456789 (){}[],\n'")
  char ','
  spaces
  value <- valueParserNoCol
  char '}'
  return (key, value)

-- column parser helper, returns list of column names
columnListParser :: Parser [String]
columnListParser = do
  char '('
  names <- sepBy1 nameParser (char ',' >> spaces)
  char ')'
  return names

-- parses the type of the column, utilized in MAKE (create) parser in which column types and names are specified
valTypeParser :: Parser ValType
valTypeParser =
  try (string "int" >> return IntType)
    <|> try (string "string" >> return StringType)

-- parses column name, type pairs
keyTypeParser :: Parser (String, ValType)
keyTypeParser = do
  char '{'
  key <- many1 letter
  char ','
  spaces
  value <- valTypeParser
  char '}'
  return (key, value)
