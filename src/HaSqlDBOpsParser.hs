module HaSqlDBOpsParser where

import HaSqlBasicParsers
import HaSqlClauseParser
import HaSqlDataParsers
import HaSqlSyntax
import Text.Parsec
import Text.Parsec.String (Parser)

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

parseView :: Parser SQLObj
parseView = do
  reservedOp "[VIEW]"
  return VIEW

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