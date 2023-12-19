module HaSqlPrettyPrint where

import Data.List
import HaSqlSyntax

prettyPrintSQLObjWithCTEs :: (SQLObj, [(String, SQLObj)]) -> String
prettyPrintSQLObjWithCTEs (sqlobj, []) = prettyPrintSQLObj sqlobj
prettyPrintSQLObjWithCTEs (sqlobj, l) =
  intercalate ",\n" (map prettyPrintCTE l)
    ++ "\n"
    ++ prettyPrintSQLObj sqlobj

prettyPrintCTE :: (String, SQLObj) -> String
prettyPrintCTE (s, sqlobj) =
  "[LET] ("
    ++ prettyPrintSQLObj sqlobj
    ++ ") [BE] ("
    ++ s
    ++ ")"

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
    VIEW -> "[VIEW]"

prettyPrintSelect :: ColumnObj -> TableObj -> Clause -> ([String], Bool) -> String
prettyPrintSelect cols table whereCls order =
  "[GET] "
    ++ prettyPrintColumnObj cols
    ++ " [IN] ("
    ++ prettyPrintTableObj table
    ++ ")"
    ++ prettyPrintClause whereCls
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
  "[REMOVEFROM] (" ++ tableName ++ ") (" ++ prettyPrintClause whereCls ++ ")"

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

prettyPrintColumnObj :: ColumnObj -> String
prettyPrintColumnObj Star = "(*)"
prettyPrintColumnObj (Columns cols) = "(" ++ intercalate ", " cols ++ ")"

prettyPrintTableObj :: TableObj -> String
prettyPrintTableObj (TName name) = name
prettyPrintTableObj (CTE cte) = prettyPrintSQLObj cte
prettyPrintTableObj (INNERJOIN table1 table2 conds) =
  prettyPrintJoin "[MATCH]" table1 table2 conds
prettyPrintTableObj (LEFTJOIN table1 table2 conds) =
  prettyPrintJoin "[MATCHLEFT]" table1 table2 conds
prettyPrintTableObj (RIGHTJOIN table1 table2 conds) =
  prettyPrintJoin "[MATCHRIGHT]" table1 table2 conds
prettyPrintTableObj (FULLJOIN table1 table2 conds) =
  prettyPrintJoin "[MATCHFULL]" table1 table2 conds
prettyPrintTableObj (NATURALJOIN table1 table2) =
  prettyPrintJoin "[MATCHREG]" table1 table2 []

prettyPrintJoin :: String -> TableObj -> TableObj -> [(String, String)] -> String
prettyPrintJoin joinType table1 table2 conds =
  "(" ++ prettyPrintTableObj table1 ++ ") " ++ joinType ++ " (" ++ prettyPrintTableObj table2 ++ ")" ++ prettyPrintJoinConditions conds

prettyPrintJoinConditions :: [(String, String)] -> String
prettyPrintJoinConditions [] = ""
prettyPrintJoinConditions conds =
  " [SUCHTHAT] " ++ " (" ++ intercalate ", " (map (\(col1, col2) -> "{" ++ col1 ++ ", " ++ col2 ++ "}") conds) ++ ")"

prettyPrintClause :: Clause -> String
prettyPrintClause NONE = ""
prettyPrintClause clause = " [IF] " ++ "(" ++ prettyPrintClauseHelper clause ++ ")"

prettyPrintClauseHelper :: Clause -> String
prettyPrintClauseHelper NONE = ""
prettyPrintClauseHelper (AND c1 c2) =
  "{ " ++ prettyPrintClauseHelper c1 ++ " AND " ++ prettyPrintClauseHelper c2 ++ "}"
prettyPrintClauseHelper (OR c1 c2) =
  "{ " ++ prettyPrintClauseHelper c1 ++ " OR " ++ prettyPrintClauseHelper c2 ++ "}"
prettyPrintClauseHelper (NOT c) = "{NOT " ++ prettyPrintClauseHelper c ++ "}"
prettyPrintClauseHelper (Clause v1 v2 op) = "{" ++ prettyPrintValue v1 ++ prettyPrintClauseOp op ++ prettyPrintValue v2 ++ "}"

prettyPrintValue :: Value -> String
prettyPrintValue (IntVal i) = show i
prettyPrintValue NilVal = "nil"
prettyPrintValue (StringVal s) = "'" ++ s ++ "'"
prettyPrintValue (ColVal c) = c

prettyPrintClauseOp :: ClauseOp -> String
prettyPrintClauseOp EQS = " == "
prettyPrintClauseOp GEQ = " >= "
prettyPrintClauseOp GTH = " > "
prettyPrintClauseOp LTH = " < "
prettyPrintClauseOp LEQ = " <= "
prettyPrintClauseOp NEQ = " != "

prettyPrintOrder :: ([String], Bool) -> String
prettyPrintOrder ([], dir) = ""
prettyPrintOrder (cols, True) =
  " [ORDER] (" ++ intercalate ", " cols ++ ") (" ++ "ascending" ++ ")"
prettyPrintOrder (cols, False) =
  " [ORDER] (" ++ intercalate ", " cols ++ ") (" ++ "descending" ++ ")"

prettyPrintHeader :: [(String, ValType)] -> String
prettyPrintHeader header =
  "(" ++ intercalate ", " (map (\(name, vtype) -> "{" ++ name ++ ", " ++ prettyPrintValType vtype ++ "}") header) ++ ")"

prettyPrintValType :: ValType -> String
prettyPrintValType StringType = "string"
prettyPrintValType IntType = "int"

prettyPrintRecord :: [(String, Value)] -> String
prettyPrintRecord record =
  "(" ++ intercalate ", " (map (\(name, value) -> "{" ++ name ++ ", " ++ prettyPrintValue value ++ "}") record) ++ ")"