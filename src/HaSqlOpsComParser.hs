module HaSqlOpsComParser where

import Data.Char (isAlphaNum)
import HaSqlSyntax
import Text.Parsec
import Text.Parsec.String (Parser)

-- parser for save, saveas, load, new, script, and quit commands (applicational commands)
parseOperationalCommand :: String -> Either ParseError Command
parseOperationalCommand = parse commandParser ""

-- tries parser for each of the possible application commands (non-CRUD operations)
commandParser :: Parser Command
commandParser = do try parseSaveAs <|> try parseSave <|> try parseLoad <|> try parseNew <|> try parseQuit <|> try parseScript <|> try parseDeleteDB

-- parses SAVE command
parseSave :: Parser Command
parseSave = string "SAVE" >> return SAVE

-- parses SAVEAS command
parseSaveAs :: Parser Command
parseSaveAs = do
  string "SAVE AS"
  spaces
  name <- many1 (satisfy isAlphaNum)
  return (SAVEAS name)

-- parses LOAD command
parseLoad :: Parser Command
parseLoad = do
  string "LOAD"
  spaces
  name <- many1 (satisfy isAlphaNum)
  return (LOAD name)

-- parses NEW command
parseNew :: Parser Command
parseNew = do
  string "NEW"
  spaces
  name <- many1 (satisfy isAlphaNum)
  return (NEW name)

-- parses QUIT command
parseQuit :: Parser Command
parseQuit = string "QUIT" >> return QUIT

-- parses SCRIPT command
parseScript :: Parser Command
parseScript = do
  string "SCRIPT"
  spaces
  name <- many1 (noneOf "0123456789 (){}[],\n'")
  return (SCRIPT name)

-- parse DELETEDB command
parseDeleteDB :: Parser Command
parseDeleteDB = do
  string "DELETEDB"
  spaces
  name <- many1 (satisfy isAlphaNum)
  return (DELETEDB name)