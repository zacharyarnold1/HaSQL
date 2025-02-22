module HaSqlTables where

import Data.List (find, intersect, nub, nubBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import HaSqlSyntax

-- performs an SQL INNER JOIN over two tables, join conditions specified by list of column name pairs, where the first value in the pair belongs to the first input table, and the second belongs to the second input table.
innerJoin :: [(String, String)] -> Table -> Table -> Table
innerJoin pairs (Table (Cols cols1) recs1) (Table (Cols cols2) recs2) =
  let joinedCols = Map.union cols1 cols2
      joinedRecs = [Rec $ Map.union (fromRecord rec1) (fromRecord rec2) | rec1 <- recs1, rec2 <- recs2, all (checkPair rec1 rec2) pairs]
   in Table (Cols joinedCols) joinedRecs

-- performs an SQL LEFT JOIN over two tables, join conditions specified by list of column name pairs, where the first value in the pair belongs to the first input table, and the second belongs to the second input table.
leftJoin :: [(String, String)] -> Table -> Table -> Table
leftJoin pairs (Table (Cols cols1) recs1) (Table (Cols cols2) recs2) =
  let joinedCols = Map.union cols1 cols2
      joinedRecs = [joinRecords rec1 (fromMaybe (nilRecord cols2) (findMatch rec1 recs2 pairs)) | rec1 <- recs1]
   in Table (Cols joinedCols) joinedRecs
  where
    joinRecords rec1 rec2 = Rec $ Map.union (fromRecord rec1) (fromRecord rec2)
    findMatch rec recs pairs = find (\r -> all (checkPair rec r) pairs) recs
    nilRecord cols = Rec $ Map.map (const NilVal) cols

-- performs an SQL RIGHT JOIN over two tables, join conditions specified by list of column name pairs, where the first value in the pair belongs to the first input table, and the second belongs to the second input table.
rightJoin :: [(String, String)] -> Table -> Table -> Table
rightJoin pairs leftTable rightTable = leftJoin (map swap pairs) rightTable leftTable

-- performs an SQL FULL JOIN over two tables, join conditions specified by list of column name pairs, where the first value in the pair belongs to the first input table, and the second belongs to the second input table.
fullJoin :: [(String, String)] -> Table -> Table -> Table
fullJoin pairs leftTable@(Table (Cols cols1) recs1) rightTable@(Table (Cols cols2) recs2) =
  let leftJoined = leftJoin pairs leftTable rightTable
      rightJoined = rightJoin pairs leftTable rightTable
      allRecords = nubBy allColsEqual (recordsFromTable leftJoined ++ recordsFromTable rightJoined)
   in Table (Cols $ Map.union cols1 cols2) allRecords
  where
    allColsEqual (Rec r1) (Rec r2) = all (\k -> Map.lookup k r1 == Map.lookup k r2) (Map.keys $ Map.union r1 r2)
    recordsFromTable (Table _ recs) = recs

-- performs an SQL NATURAL JOIN over two tables, join conditions specified by list of column name pairs, where the first value in the pair belongs to the first input table, and the second belongs to the second input table.
naturalJoin :: Table -> Table -> Table
naturalJoin (Table (Cols cols1) recs1) (Table (Cols cols2) recs2) =
  let commonCols = intersect (Map.keys cols1) (Map.keys cols2)
      joinedCols = Map.union cols1 cols2
      joinedRecs = [Rec $ Map.union (fromRecord rec1) (fromRecord rec2) | rec1 <- recs1, rec2 <- recs2, all (checkCommon rec1 rec2) commonCols]
   in Table (Cols joinedCols) joinedRecs

-- Determines wether two records should be joined, given a column name pair that represents a join condition
checkPair :: Record -> Record -> (String, String) -> Bool
checkPair rec1 rec2 (col1, col2) =
  Map.lookup col1 (fromRecord rec1) == Map.lookup col2 (fromRecord rec2)

-- checks wether two records contain a common column
checkCommon :: Record -> Record -> String -> Bool
checkCommon (Rec rec1) (Rec rec2) col =
  Map.lookup col rec1 == Map.lookup col rec2

-- swaps elements of a tuple
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
