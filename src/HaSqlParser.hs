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
    ops = ["[GET]", "[IN]", "[IF]", "[MAKE]", "[PUT]", "[CHANGE]", "[TO]", "[WITH]", "[REMOVEFROM]", "[LET]", "[BE]", "[COMBINE]", "[ORDER]", "[MATCH]", "[MATCHLEFT]", "[MATCHRIGHT]", "[MATCHFULL]", "[MATCHREG]", "[SUCHTHAT]", "[REMOVE]", "[ADD COLS]", "[RENAME COL]", "[RENAME TABLE]", "[DELETE TABLE]", "[VIEW]"]
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
mainParse :: String -> Either ParseError (SQLObj, [(String, SQLObj)])
mainParse = parse parseDSLWithCTEs ""

-- parses optional CTEs, and then calls dsl parser on remaining input
parseDSLWithCTEs :: Parser (SQLObj, [(String, SQLObj)])
parseDSLWithCTEs = do
  ctes <- sepBy parseCTE (char ',' >> spaces)
  spaces
  sqlobj <- dslParser
  return (sqlobj, ctes)

-- tries parser for each of the 5 possible operations (CRUD operations)
dslParser :: Parser SQLObj
dslParser = try parseSelect <|> try parseCreate <|> try parseInsert <|> try parseUpdate <|> try parseDelete <|> try parseAdd <|> try parseDropTable <|> try parseRenameCol <|> parseRenameTable <|> parseView

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

parseView :: Parser SQLObj
parseView = do
  reservedOp "[VIEW]"
  return VIEW

-- parses SORT (orderby) statement
parseOrdering :: Parser ([String], Bool)
parseOrdering = do
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

-- parses MAKE (create) statement
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

-- parses PUT (insert) statement
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

-- parses CHANGE (update statement)
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

-- parses REMOVEFROM (delete statement)
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

-- parse ADD command, adds column
parseAdd :: Parser SQLObj
parseAdd = do
  reservedOp "[ADD COLS]"
  spaces
  cols <- parseHeader
  spaces
  reservedOp "[TO]"
  spaces
  char '('
  table <- nameParser
  char ')'
  return ADD {tableName = table, newCols = cols}

-- parse RENAME TABLE command
parseRenameTable :: Parser SQLObj
parseRenameTable = do
  reservedOp "[RENAME TABLE]"
  spaces
  char '('
  oldName <- nameParser
  char ')'
  spaces
  reservedOp "[TO]"
  char '('
  newName <- nameParser
  char ')'
  return RENAMETABLE {tableName = oldName, newTableName = newName}

-- parse RENAME COL command
parseRenameCol :: Parser SQLObj
parseRenameCol = do
  reservedOp "[RENAME COL]"
  spaces
  char '('
  oldColName <- nameParser
  char ')'
  spaces
  reservedOp "[TO]"
  char '('
  newColName <- nameParser
  char ')'
  spaces
  reservedOp "[IN]"
  spaces
  char '('
  tableName <- nameParser
  char ')'
  return RENAMECOL {tableName = tableName, oldColName = oldColName, newColName = newColName}

-- parse DELETE TABLE (drop) command
parseDropTable :: Parser SQLObj
parseDropTable = do
  reservedOp "[DELETE TABLE]"
  spaces
  char '('
  table <- nameParser
  char ')'
  return DROP {tableName = table}

-- parses table, which could involve joins, a nested CTE, or solely a table name - tries each of the appropriate parsers
parseTable :: Parser TableObj
parseTable = try parseJoin <|> try parseCTEalt <|> parseTableName

-- parses nested CTEs, i.e. CTE's within queries rather than those that preceed queries
parseCTEalt :: Parser TableObj
parseCTEalt = do CTE <$> parseSelect

-- parses table name
parseTableName :: Parser TableObj
parseTableName = do
  name <- many1 (noneOf "0123456789 (){}[],\n'") -- Adjust according to your naming conventions
  return $ TName name

-- parses Join statements
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

-- parses column names
parseColumns :: Parser ColumnObj
parseColumns =
  try (string "(*)" >> return Star)
    <|> try (Columns <$> columnListParser)

-- column parser helper, returns list of column names
columnListParser :: Parser [String]
columnListParser = do
  char '('
  names <- sepBy1 nameParser (char ',' >> spaces)
  char ')'
  return names

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

-- parses ValTypes (NilVal, IntVal, StringVal, ColVal)
valueParser :: Parser Value
valueParser =
  try (string "Nil" >> return NilVal)
    <|> try (intParser >>= return . IntVal)
    <|> try (stringLiteralParser >>= return . StringVal)
    <|> try (nameParser >>= return . ColVal)

-- parses column, value pairs, helper to parseRecord
keyValueParser :: Parser (String, Value)
keyValueParser = do
  char '{'
  key <- many1 letter
  char ','
  spaces
  value <- valueParser
  char '}'
  return (key, value)

-- parses list of column, value pairs, utilized in CHANGE (update) parser
parseRecord :: Parser [(String, Value)]
parseRecord = do
  char '('
  pairs <- sepBy keyValueParser (char ',' >> spaces)
  char ')'
  return pairs

-- same as valueParser, but does not support ColVals
valueParserNoCol :: Parser Value
valueParserNoCol =
  try (string "Nil" >> return NilVal)
    <|> try (intParser >>= return . IntVal)
    <|> try (stringLiteralParser >>= return . StringVal)

-- parses column, value pairs, helper to parseRecordNoCol
keyValueParserNoCol :: Parser (String, Value)
keyValueParserNoCol = do
  char '{'
  key <- many1 letter
  char ','
  spaces
  value <- valueParserNoCol
  char '}'
  return (key, value)

-- -parses list of column, value pairs, utilized in PUT (update) parser (ColVals not supported as values, only Nils, Ints, and Strings)
parseRecordNoCol :: Parser [(String, Value)]
parseRecordNoCol = do
  char '('
  pairs <- sepBy keyValueParserNoCol (char ',' >> spaces)
  char ')'
  return pairs

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

-- parses table headers, i.e the schema of the table, list of column name, type pairs
parseHeader :: Parser [(String, ValType)]
parseHeader = do
  char '('
  pairs <- sepBy keyTypeParser (char ',' >> spaces)
  char ')'
  return pairs

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

parseDeleteDB :: Parser Command
parseDeleteDB = do
  string "DELETEDB"
  spaces
  name <- many1 (satisfy isAlphaNum)
  return (DELETEDB name)

prettyPrintSQLObj :: SQLObj -> String
prettyPrintSQLObj obj =
  case obj of
    SELECT cols table whereCls order -> prettyPrintSelect cols table whereCls order
    CREATE tableName header -> prettyPrintCreate tableName header
    INSERT tableName record -> prettyPrintInsert tableName record
    UPDATE tableName record whereCls -> prettyPrintUpdate tableName record whereCls
    DELETE tableName whereCls -> prettyPrintDelete tableName whereCls
    ADD tableName newCols -> prettyPrintAdd tableName newCols
    RENAMETABLE oldName newName -> prettyPrintRenameTable oldName newName
    RENAMECOL tableName oldColName newColName -> prettyPrintRenameCol tableName oldColName newColName
    DROP tableName -> prettyPrintDropTable tableName

-- Add cases for other SQLObj types if needed

prettyPrintSelect :: ColumnObj -> TableObj -> Clause -> ([String], Bool) -> String
prettyPrintSelect cols table whereCls order =
  "[GET] "
    ++ prettyPrintColumnObj cols
    ++ " [IN] ("
    ++ prettyPrintTableObj table
    ++ ") "
    ++ prettyPrintClause whereCls
    ++ " "
    ++ prettyPrintOrder order

prettyPrintCreate :: String -> [(String, ValType)] -> String
prettyPrintCreate tableName header =
  "[MAKE] (" ++ tableName ++ ") [WITH] " ++ prettyPrintHeader header

prettyPrintInsert :: String -> [(String, Value)] -> String
prettyPrintInsert tableName record =
  "[PUT] (" ++ tableName ++ ") [WITH] " ++ prettyPrintRecord record

prettyPrintUpdate :: String -> [(String, Value)] -> Clause -> String
prettyPrintUpdate tableName record whereCls =
  "[CHANGE] ("
    ++ tableName
    ++ ") [TO] "
    ++ prettyPrintRecord record
    ++ " "
    ++ prettyPrintClause whereCls

prettyPrintDelete :: String -> Clause -> String
prettyPrintDelete tableName whereCls =
  "[REMOVEFROM] (" ++ tableName ++ ") " ++ prettyPrintClause whereCls

prettyPrintAdd :: String -> [(String, ValType)] -> String
prettyPrintAdd tableName newCols =
  "[ADD COLS] " ++ prettyPrintHeader newCols ++ " [TO] (" ++ tableName ++ ")"

prettyPrintRenameTable :: String -> String -> String
prettyPrintRenameTable oldName newName =
  "[RENAME TABLE] (" ++ oldName ++ ") [TO] (" ++ newName ++ ")"

prettyPrintRenameCol :: String -> String -> String -> String
prettyPrintRenameCol tableName oldColName newColName =
  "[RENAME COL] (" ++ oldColName ++ ") [TO] (" ++ newColName ++ ") [IN] (" ++ tableName ++ ")"

prettyPrintDropTable :: String -> String
prettyPrintDropTable tableName =
  "[DELETE TABLE] (" ++ tableName ++ ")"

-- Add helper functions to pretty print components like ColumnObj, TableObj, Clause, etc.
prettyPrintColumnObj :: ColumnObj -> String
prettyPrintColumnObj Star = "(*)"
prettyPrintColumnObj (Columns cols) = "(" ++ intercalate ", " cols ++ ")"

prettyPrintTableObj :: TableObj -> String
prettyPrintTableObj (TName name) = name
prettyPrintTableObj (CTE cte) = prettyPrintSQLObj cte
prettyPrintTableObj (INNERJOIN table1 table2 conds) =
  prettyPrintJoin "INNER JOIN" table1 table2 conds
prettyPrintTableObj (LEFTJOIN table1 table2 conds) =
  prettyPrintJoin "LEFT JOIN" table1 table2 conds
prettyPrintTableObj (RIGHTJOIN table1 table2 conds) =
  prettyPrintJoin "RIGHT JOIN" table1 table2 conds
prettyPrintTableObj (FULLJOIN table1 table2 conds) =
  prettyPrintJoin "FULL JOIN" table1 table2 conds
prettyPrintTableObj (NATURALJOIN table1 table2) =
  prettyPrintJoin "NATURAL JOIN" table1 table2 []

prettyPrintJoin :: String -> TableObj -> TableObj -> [(String, String)] -> String
prettyPrintJoin joinType table1 table2 conds =
  "(" ++ prettyPrintTableObj table1 ++ ") " ++ joinType ++ " (" ++ prettyPrintTableObj table2 ++ ")" ++ prettyPrintJoinConditions conds

prettyPrintJoinConditions :: [(String, String)] -> String
prettyPrintJoinConditions [] = ""
prettyPrintJoinConditions conds =
  " ON " ++ intercalate " AND " (map (\(col1, col2) -> col1 ++ " = " ++ col2) conds)

-- Add cases for JOINs if needed

prettyPrintClause :: Clause -> String
prettyPrintClause NONE = ""
prettyPrintClause clause = "[IF] " ++ show clause -- Modify according to how you want to represent clauses

prettyPrintOrder :: ([String], Bool) -> String
prettyPrintOrder (cols, dir) =
  "[ORDER] " ++ intercalate ", " cols ++ " (" ++ show dir ++ ")"

prettyPrintHeader :: [(String, ValType)] -> String
prettyPrintHeader header =
  "(" ++ intercalate ", " (map (\(name, vtype) -> "{" ++ name ++ ", " ++ show vtype ++ "}") header) ++ ")"

prettyPrintRecord :: [(String, Value)] -> String
prettyPrintRecord record =
  "(" ++ intercalate ", " (map (\(name, value) -> "{" ++ name ++ ", " ++ show value ++ "}") record) ++ ")"

-- Note: This is a basic structure. You might need to modify or extend it to match your exact requirements and data types.
