module HaSqlScriptValidator where

import Data.Bifunctor (second)
import Data.Map qualified as Map
import HaSqlBasicParsers
import Text.Parsec
import Text.Parsec.String (Parser)

data ValueType = IntType | StringType deriving (Show, Eq)

type Schema = Map.Map String ValueType

type DatabaseState = Map.Map String Schema

-- Parses a MAKE command
parseMake :: Parser (Either (String, Schema) (String, [(String, ValueType)]))
parseMake = do
  reservedOp "[MAKE]"
  spaces
  char '('
  tableName <- many1 (noneOf "0123456789 (){}[],\n'")
  char ')'
  spaces
  reservedOp "[WITH]"
  spaces
  char '('
  schema <- sepBy HaSqlScriptValidator.keyTypeParser (try $ string ", ")
  char ')'
  return $ Left (tableName, Map.fromList schema)

-- Parses a PUT command
parsePut :: Parser (Either (String, Schema) (String, [(String, ValueType)]))
parsePut = do
  reservedOp "[PUT]"
  spaces
  char '('
  tableName <- many1 (noneOf "0123456789 (){}[],\n'")
  char ')'
  spaces
  reservedOp "[WITH]"
  spaces
  char '('
  values <- sepBy parseValue (try $ string ", ")
  char ')'
  return $ Right (tableName, values)

-- Helper to parse a single value
parseValue :: Parser (String, ValueType)
parseValue = do
  char '{'
  columnName <- many1 $ noneOf "0123456789 (){}[],\n'"
  char ','
  spaces
  value <- HaSqlScriptValidator.valueParser
  char '}'
  return (columnName, value)

-- Parser to determine if a value is of type Int or String
valueParser :: Parser ValueType
valueParser =
  try (stringLiteral >> return StringType)
    <|> try (intLiteral >> return IntType)

-- Parses an integer literal
intLiteral :: Parser String
intLiteral = many1 digit

-- Parses a string literal
stringLiteral :: Parser String
stringLiteral = char '\'' *> many (noneOf "\'") <* char '\''

keyTypeParser :: Parser (String, ValueType)
keyTypeParser = do
  char '{'
  key <- many1 (noneOf "0123456789 (){}[],\n'")
  char ','
  spaces
  value <- HaSqlScriptValidator.valTypeParser
  char '}'
  return (key, value)

valTypeParser :: Parser ValueType
valTypeParser =
  try (string "int" >> return IntType)
    <|> try (string "string" >> return StringType)

-- Parses either a MAKE or PUT command
parseCommand :: Parser (Either (String, Schema) (String, [(String, ValueType)]))
parseCommand = parseMake <|> parsePut

validateAndExecute :: DatabaseState -> Either (String, Schema) (String, [(String, ValueType)]) -> Either String DatabaseState
validateAndExecute dbState (Left (tableName, schema)) =
  if Map.member tableName dbState
    then Left $ "Table " ++ tableName ++ " already exists."
    else Right $ Map.insert tableName schema dbState
validateAndExecute dbState (Right (tableName, entries)) =
  case Map.lookup tableName dbState of
    Just schema ->
      if all (\(col, valType) -> Map.lookup col schema == Just valType) entries
        then Right dbState
        else Left "Type mismatch or column doesn't exist in table."
    Nothing -> Left $ "Table " ++ tableName ++ " does not exist."

-- Parses a line of the script, either a MAKE or PUT command
parseScriptLine :: Parser (Either (String, Schema) (String, [(String, ValueType)]))
parseScriptLine = do
  command <- parseCommand
  optional newline
  return command

-- Validates the script from a file, processing each line separately
validateScriptFromFile :: String -> IO (Either String String)
validateScriptFromFile filename = do
  content <- readFile filename
  let linesOfScript = lines content
  let parsedCommands = zipWith (\lineNum line -> (lineNum, parse parseScriptLine filename line)) [1 ..] linesOfScript
  return $ processCommands parsedCommands Map.empty

processCommands :: [(Int, Either ParseError (Either (String, Schema) (String, [(String, ValueType)])))] -> DatabaseState -> Either String String
processCommands [] _ = Right "Script is valid."
processCommands ((lineNum, parsedCmd) : cmds) dbState = case parsedCmd of
  Left parseErr -> Left $ "Syntax error on line " ++ show lineNum ++ ": " ++ show parseErr
  Right cmd -> case validateAndExecute dbState cmd of
    Left errMsg -> Left $ "Invalid command on line " ++ show lineNum ++ ": " ++ errMsg
    Right newDbState -> processCommands cmds newDbState
