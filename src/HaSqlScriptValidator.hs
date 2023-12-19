module HaSqlScriptValidator where

import qualified Data.Map as Map
import HaSqlBasicParsers
import Text.Parsec
import Text.Parsec.String (Parser)

data ValType = IntType | StringType deriving (Show, Eq)

type Schema = Map.Map String ValType

type DatabaseState = Map.Map String Schema

-- Parses a MAKE command
parseMake :: Parser (String, Schema)
parseMake = do
  try $ string "[MAKE]"
  spaces
  tableName <- between (char '(') (char ')') (many1 $ noneOf "0123456789 (){}[],\n'")
  spaces
  string "[WITH]"
  spaces
  schema <- between (char '(') (char ')') (sepBy keyTypeParser (try $ string ", "))
  return (tableName, Map.fromList schema)

-- Parses a PUT command
parsePut :: Parser (String, [(String, String)])
parsePut = do
  try $ string "[PUT]"
  spaces
  tableName <- between (char '(') (char ')') (many1 $ noneOf "0123456789 (){}[],\n'")
  spaces
  string "[WITH]"
  spaces
  values <- between (char '(') (char ')') (sepBy parseValue (try $ string ", "))
  return (tableName, values)

-- Helper to parse a single value
parseValue :: Parser (String, String)
parseValue = do
  char '{'
  columnName <- many1 $ noneOf "0123456789 (){}[],\n'"
  char ','
  spaces
  value <- many1 (noneOf "}")
  char '}'
  return (columnName, value)

parseCommand :: Parser (Either (String, Schema) (String, [(String, ValType)]))
parseCommand = do
  commandType <- many1 $ noneOf "0123456789(){},\n'"
  case commandType of
    "[MAKE]" -> Left <$> parseMake
    "[PUT]" -> Right <$> parsePut
    _ -> fail $ "Please optimize script. Script should not contain commands of type " ++ commandType

validateAndExecute :: DatabaseState -> Either (String, Schema) (String, [(String, ValType)]) -> Either String DatabaseState
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
parseScriptLine :: Parser (Either (String, Schema) (String, [(String, ValType)]))
parseScriptLine = do
  command <- parseCommand
  optional newline
  return command

-- Validates the script from a file, processing each line separately
validateScriptFromFile :: String -> IO (Either String DatabaseState)
validateScriptFromFile filename = do
  content <- readFile ("scripts/" ++ filename)
  let linesOfScript = zip [1 ..] $ lines content
  let parsedLines = map (parse parseScriptLine filename) linesOfScript
  return $ foldl processLine (Right (Map.empty, 1)) parsedLines
  where
    processLine :: Either String (DatabaseState, Int) -> Either ParseError (Either (String, Schema) (String, [(String, ValType)])) -> Either String (DatabaseState, Int)
    processLine acc@(Left _) _ = acc
    processLine (Right (dbState, lineNum)) parsedLine =
      case parsedLine of
        Left parseErr -> Left $ "Syntax error on line " ++ show lineNum
        Right cmd ->
          case validateAndExecute dbState cmd of
            Left errMsg -> Left $ "Invalid command. Database state compromised on line " ++ show lineNum
            Right newDbState -> Right (newDbState, lineNum + 1)