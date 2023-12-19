module HaSqlTableParser where

import HaSqlBasicParsers
import HaSqlClauseParser
import HaSqlDBOpsParser
import HaSqlDataParsers
import HaSqlSyntax
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as Token

parseTable :: Parser TableObj
parseTable = try parseJoin <|> try parseCTEalt <|> parseTableName

-- parses nested CTEs, i.e. CTE's within queries rather than those that preceed queries
parseCTEalt :: Parser TableObj
parseCTEalt = do CTE <$> parseSelect

-- parses GET (select) statement
parseSelect :: Parser SQLObj
parseSelect = do
  reservedOp "[GET]"
  spaces
  cols <- parseColumns
  spaces
  reservedOp "[IN]"
  spaces
  char '('
  table <- parseTable
  char ')'
  spaces
  whereClause <- option NONE parseClause
  spaces
  ordering <- option ([], False) parseOrdering
  return SELECT {selectColumns = cols, fromTable = table, whereClauses = whereClause, order = ordering}

-- parses joins
parseJoin :: Parser TableObj
parseJoin = try parseInnerJoin <|> try parseLeftJoin <|> try parseRightJoin <|> try parseFullJoin <|> parseNaturalJoin

-- parses MATCH (INNER JOIN) statements
parseInnerJoin :: Parser TableObj
parseInnerJoin = do
  char '('
  table1 <- parseTable
  char ')'
  spaces
  reservedOp "[MATCH]"
  spaces
  char '('
  table2 <- parseTable
  char ')'
  spaces
  reservedOp "[SUCHTHAT]"
  spaces
  char '('
  conds <- sepBy parseCondition (char ',' >> spaces)
  char ')'
  return $ INNERJOIN table1 table2 conds

-- parses MATCHLEFT (LEFT JOIN) statements
parseLeftJoin :: Parser TableObj
parseLeftJoin = do
  char '('
  table1 <- parseTable
  char ')'
  spaces
  reservedOp "[MATCHLEFT]"
  spaces
  char '('
  table2 <- parseTable
  char ')'
  spaces
  reservedOp "[SUCHTHAT]"
  spaces
  char '('
  conds <- sepBy parseCondition (char ',' >> spaces)
  char ')'
  return $ LEFTJOIN table1 table2 conds

-- parses MATCHRIGHT (RIGHT JOIN) statements
parseRightJoin :: Parser TableObj
parseRightJoin = do
  char '('
  table1 <- parseTable
  char ')'
  spaces
  reservedOp "[MATCHRIGHT]"
  spaces
  char '('
  table2 <- parseTable
  char ')'
  spaces
  reservedOp "[SUCHTHAT]"
  spaces
  char '('
  conds <- sepBy parseCondition (char ',' >> spaces)
  char ')'
  return $ RIGHTJOIN table1 table2 conds

-- parses MATCHFULL (FULL JOIN) statements
parseFullJoin :: Parser TableObj
parseFullJoin = do
  char '('
  table1 <- parseTable
  char ')'
  spaces
  reservedOp "[MATCHFULL]"
  spaces
  char '('
  table2 <- parseTable
  char ')'
  spaces
  reservedOp "[SUCHTHAT]"
  spaces
  char '('
  conds <- sepBy parseCondition (char ',' >> spaces)
  char ')'
  return $ FULLJOIN table1 table2 conds

-- parses MATCHREG (NATURAL JOIN) statements
parseNaturalJoin :: Parser TableObj
parseNaturalJoin = do
  char '('
  table1 <- parseTable
  char ')'
  spaces
  reservedOp "[MATCHREG]"
  spaces
  char '('
  table2 <- parseTable
  char ')'
  return $ NATURALJOIN table1 table2

-- parses single conditional clauses
parseCondition :: Parser (String, String)
parseCondition = do
  char '{'
  col1 <- many1 (noneOf "0123456789 (){}[],\n'")
  char ','
  spaces
  col2 <- many1 (noneOf "0123456789 (){}[],\n'")
  char '}'
  return (col1, col2)
