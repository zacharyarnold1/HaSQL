module HaSqlMainParser where

import HaSqlBasicParsers
import HaSqlDBOpsParser
import HaSqlSyntax
import HaSqlTableParser
import Text.Parsec
import Text.Parsec.String (Parser)

-- Main parse function for the DSL
mainParse :: String -> Either ParseError (SQLObj, [(String, SQLObj)])
mainParse = parse parseDSLWithCTEs ""

-- tries parser for each of the 5 possible operations (CRUD operations)
dslParser :: Parser SQLObj
dslParser = try parseSelect <|> try parseCreate <|> try parseInsert <|> try parseUpdate <|> try parseDelete <|> try parseAdd <|> try parseDropTable <|> try parseRenameCol <|> parseRenameTable <|> parseView

-- parses optional CTEs, and then calls dsl parser on remaining input
parseDSLWithCTEs :: Parser (SQLObj, [(String, SQLObj)])
parseDSLWithCTEs = do
  ctes <- sepBy parseCTE (char ',' >> spaces)
  spaces
  sqlobj <- dslParser
  return (sqlobj, ctes)

-- parser for common table expressions
parseCTE :: Parser (String, SQLObj)
parseCTE = do
  reservedOp "[LET]"
  spaces
  char '('
  sel <- parseSelect
  char ')'
  spaces
  reservedOp "[BE]"
  spaces
  char '('
  tableName <- nameParser
  char ')'
  return (tableName, sel)