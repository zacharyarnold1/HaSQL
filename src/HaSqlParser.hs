module HaSqlParser where

import Data.Char (isAlphaNum)
import Data.List (intercalate)
import HaSqlSyntax
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as Token

-- Define a language lexer
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    ops = ["[GET]", "[IN]", "[IF]", "[MAKE]", "[PUT]", "[CHANGE]", "[TO]", "[DELETE]", "[WITH]", "[REMOVEFROM]", "[LET]", "[BE]", "[COMBINE]", "[ORDER]"]
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

-- Main parse function for the DSL
parseDSL :: String -> Either ParseError SQLObj
parseDSL input = parse dslParser "" input

dslParserWithCTEs :: Parser (SQLObj, [(String, SQLObj)])
dslParserWithCTEs = do
  ctes <- sepBy parseCTE (char ',' >> spaces)
  spaces
  sqlobj <- dslParser
  return (sqlobj, ctes)

dslParser :: Parser SQLObj
dslParser = try parseSelect <|> try parseCreate <|> try parseInsert <|> try parseUpdate <|> parseDelete

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
  tableName <- nameParser
  return (tableName, sel)

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
  groupClause <- option [] parseGrouping
  ordering <- option ([], False) parseOrdering
  return SELECT {selectColumns = cols, fromTable = table, whereClauses = whereClause, grouping = groupClause, order = ordering}

parseGrouping :: Parser [String]
parseGrouping = do
  spaces
  reservedOp "[COMBINE]"
  spaces
  cols <- columnListParser
  return cols
  
parseOrdering :: Parser ([String], Bool)
parseOrdering = do
  spaces 
  reservedOp "[ORDER]"
  spaces
  cols <- columnListParser
  spaces
  char '('
  direction <- nameParser
  char ')'
  let bool = case direction of
               "ascending" -> True
               "descending" -> False
               _ -> error "Invalid ordering direction"
  return (cols, bool)

parseCreate :: Parser SQLObj
parseCreate = do
  reservedOp "[MAKE]"
  spaces
  char '('
  table <- nameParser
  char ')'
  spaces
  reservedOp "[WITH]"
  spaces
  header <- parseHeader
  return CREATE {tableName = table, header = header}

parseInsert :: Parser SQLObj
parseInsert = do
  reservedOp "[PUT]"
  spaces
  char '('
  table <- nameParser
  char ')'
  spaces
  reservedOp "[WITH]"
  spaces
  record <- parseRecordNoCol
  return INSERT {tableName = table, record = record}

parseUpdate :: Parser SQLObj
parseUpdate = do
  reservedOp "[CHANGE]"
  spaces
  char '('
  table <- nameParser
  char ')'
  spaces
  reservedOp "[TO]"
  spaces
  record <- parseRecord
  spaces
  clause <- option NONE parseClause
  return UPDATE {tableName = table, record = record, updateWhere = clause}

parseDelete :: Parser SQLObj
parseDelete = do
  reservedOp "[REMOVEFROM]"
  spaces
  char '('
  table <- nameParser
  char ')'
  spaces
  clause <- option NONE parseClause
  return DELETE {deleteFrom = table, deleteWhere = clause}

parseTable :: Parser TableObj
parseTable = try parseJoin <|> try parseCTEalt <|> parseTableName

parseCTEalt :: Parser TableObj
parseCTEalt = do CTE <$> parseSelect

parseTableName :: Parser TableObj
parseTableName = do
  name <- many1 (noneOf "0123456789 (){}[],\n'") -- Adjust according to your naming conventions
  return $ TName name

parseJoin :: Parser TableObj
parseJoin = try parseInnerJoin <|> try parseLeftJoin <|> try parseRightJoin <|> try parseFullJoin <|> parseNaturalJoin

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
  reservedOp "[ON]"
  spaces
  char '('
  conds <- sepBy parseCondition (char ',' >> spaces)
  char ')'
  return $ INNERJOIN table1 table2 conds

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
  reservedOp "[ON]"
  spaces
  char '('
  conds <- sepBy parseCondition (char ',' >> spaces)
  char ')'
  return $ LEFTJOIN table1 table2 conds

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
  reservedOp "[ON]"
  spaces
  char '('
  conds <- sepBy parseCondition (char ',' >> spaces)
  char ')'
  return $ RIGHTJOIN table1 table2 conds

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
  reservedOp "[ON]"
  spaces
  char '('
  conds <- sepBy parseCondition (char ',' >> spaces)
  char ')'
  return $ FULLJOIN table1 table2 conds

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

parseCondition :: Parser (String, String)
parseCondition = do
  char '{'
  col1 <- many1 (noneOf "0123456789 (){}[],\n'")
  char ','
  spaces
  col2 <- many1 (noneOf "0123456789 (){}[],\n'")
  char '}'
  return (col1, col2)


-- Placeholder functions (to be implemented)
parseColumns :: Parser ColumnObj
parseColumns =
  try (string "(*)" >> return Star)
    <|> try (Columns <$> columnListParser)

columnListParser :: Parser [String]
columnListParser = do
  char '('
  names <- sepBy1 nameParser (char ',' >> spaces)
  char ')'
  return names

intParser :: Parser Int
intParser = do
  digits <- many1 digit
  return (read digits)

stringLiteralParser :: Parser String
stringLiteralParser = do
  char '\''
  str <- many (noneOf "'")
  char '\''
  return str

nameParser :: Parser String
nameParser = do
  many1 (noneOf "0123456789 (){}[],\n'")

valueParser :: Parser Value
valueParser =
  try (string "Nil" >> return NilVal)
    <|> try (intParser >>= return . IntVal)
    <|> try (stringLiteralParser >>= return . StringVal)
    <|> try (nameParser >>= return . ColVal)

keyValueParser :: Parser (String, Value)
keyValueParser = do
  char '{'
  key <- many1 letter
  char ','
  spaces
  value <- valueParser
  char '}'
  return (key, value)

parseRecord :: Parser [(String, Value)]
parseRecord = do
  char '('
  pairs <- sepBy keyValueParser (char ',' >> spaces)
  char ')'
  return pairs

valueParserNoCol :: Parser Value
valueParserNoCol =
  try (string "Nil" >> return NilVal)
    <|> try (intParser >>= return . IntVal)
    <|> try (stringLiteralParser >>= return . StringVal)

keyValueParserNoCol :: Parser (String, Value)
keyValueParserNoCol = do
  char '{'
  key <- many1 letter
  char ','
  spaces
  value <- valueParserNoCol
  char '}'
  return (key, value)

parseRecordNoCol :: Parser [(String, Value)]
parseRecordNoCol = do
  char '('
  pairs <- sepBy keyValueParserNoCol (char ',' >> spaces)
  char ')'
  return pairs

valTypeParser :: Parser ValType
valTypeParser =
  try (string "int" >> return IntType)
    <|> try (string "integer" >> return IntType)
    <|> try (string "string" >> return StringType)

keyTypeParser :: Parser (String, ValType)
keyTypeParser = do
  char '{'
  key <- many1 letter
  char ','
  spaces
  value <- valTypeParser
  char '}'
  return (key, value)

parseHeader :: Parser [(String, ValType)]
parseHeader = do
  char '('
  pairs <- sepBy keyTypeParser (char ',' >> spaces)
  char ')'
  return pairs

operatorParser :: Parser ClauseOp
operatorParser =
  try (string "==" >> return EQS)
    <|> try (string ">=" >> return GEQ)
    <|> try (string ">" >> return GTH)
    <|> try (string "<=" >> return LEQ)
    <|> try (string "<" >> return LTH)
    <|> try (string "!=" >> return NEQ)

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

parseClause :: Parser Clause
parseClause = do
  reservedOp "[IF]"
  spaces
  char '('
  clause <- clauseParser
  char ')'
  return clause

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

parseCommand :: String -> Either ParseError Command
parseCommand input = parse commandParser "" input

commandParser :: Parser Command
commandParser = do try parseSaveAs <|> try parseSave <|> try parseLoad <|> try parseNew <|> try parseQuit <|> try parseScript

parseSave :: Parser Command
parseSave = string "SAVE" >> return SAVE

parseSaveAs :: Parser Command
parseSaveAs = do
  string "SAVE AS"
  spaces
  name <- many1 (satisfy isAlphaNum)
  return (SAVEAS name)

parseLoad :: Parser Command
parseLoad = do
  string "LOAD"
  spaces
  name <- many1 (satisfy isAlphaNum)
  return (LOAD name)

parseNew :: Parser Command
parseNew = do
  string "NEW"
  spaces
  name <- many1 (satisfy isAlphaNum)
  return (NEW name)

parseQuit :: Parser Command
parseQuit = string "QUIT" >> return QUIT

parseScript :: Parser Command
parseScript = do
  string "SCRIPT"
  spaces
  name <- many1 (noneOf "0123456789 (){}[],\n'")
  return (SCRIPT name)

-- prettyPrintSQLObj :: SQLObj -> String
-- prettyPrintSQLObj (SELECT cols table whereCls) =
--   "[GET] "
--     ++ prettyPrintColumnObj cols
--     ++ " [IN] ("
--     ++ table
--     ++ ") "
--     ++ prettyPrintClause whereCls
-- prettyPrintSQLObj (CREATE table header) =
--   "[MAKE] (" ++ table ++ ") [WITH] " ++ prettyPrintHeader header
-- prettyPrintSQLObj (INSERT table record) =
--   "[PUT] (" ++ table ++ ") [WITH] " ++ prettyPrintRecord record
-- prettyPrintSQLObj (UPDATE table record whereCls) =
--   "[CHANGE] ("
--     ++ table
--     ++ ") [TO] "
--     ++ prettyPrintRecord record
--     ++ " "
--     ++ prettyPrintClause whereCls
-- prettyPrintSQLObj (DELETE table whereCls) =
--   "[REMOVEFROM] (" ++ table ++ ") " ++ prettyPrintClause whereCls

-- prettyPrintColumnObj :: ColumnObj -> String
-- prettyPrintColumnObj Star = "(*)"
-- prettyPrintColumnObj (Columns xs) = intercalate ", " xs

-- prettyPrintClause :: Clause -> String
-- prettyPrintClause v1 v2 co = prettyPrintValue v1 ++ prettyPrintClauseOp co ++ prettyPrintValue v2

-- prettyPrintClauseOp :: ClauseOp -> String
-- prettyPrintClauseOp EQS = " == "
-- prettyPrintClauseOp GEQ = " >= "
-- prettyPrintClauseOp GTH = " > "
-- prettyPrintClauseOp LEQ = " <= "
-- prettyPrintClauseOp LTH = " < "
-- prettyPrintClauseOp NEQ = " /= "