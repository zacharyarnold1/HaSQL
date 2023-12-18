module HaSqlClauseParser where

import HaSqlBasicParsers
import HaSqlSyntax
import Text.Parsec
import Text.Parsec.String (Parser)

-- parses clause operators
operatorParser :: Parser ClauseOp
operatorParser =
  try (string "==" >> return EQS)
    <|> try (string ">=" >> return GEQ)
    <|> try (string ">" >> return GTH)
    <|> try (string "<=" >> return LEQ)
    <|> try (string "<" >> return LTH)
    <|> try (string "!=" >> return NEQ)

-- parses individual clauses, returns Clause object
singleClauseParser :: Parser Clause
singleClauseParser = do
  char '{'
  spaces
  firstValue <- valueParser
  spaces
  op <- operatorParser
  spaces
  secondValue <- valueParser
  spaces
  char '}'
  return (Clause firstValue secondValue op)

-- parses clause
parseClause :: Parser Clause
parseClause = do
  reservedOp "[IF]"
  spaces
  char '('
  clause <- clauseParser
  char ')'
  return clause

-- recursive function to parse entire clauses, which can contain nested clauses
clauseParser :: Parser Clause
clauseParser =
  try singleClauseParser
    <|> try
      ( do
          char '{'
          spaces
          leftClause <- clauseParser
          spaces
          string "AND"
          spaces
          rightClause <- clauseParser
          spaces
          char '}'
          return (AND leftClause rightClause)
      )
    <|> try
      ( do
          char '{'
          spaces
          leftClause <- clauseParser
          spaces
          string "OR"
          spaces
          rightClause <- clauseParser
          spaces
          char '}'
          return (OR leftClause rightClause)
      )
    <|> try
      ( do
          char '{'
          spaces
          string "NOT"
          spaces
          subClause <- clauseParser
          spaces
          char '}'
          return (NOT subClause)
      )
