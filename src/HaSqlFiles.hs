module HaSqlFiles where

import Control.Applicative (Alternative (..))
import Data.List (isPrefixOf, stripPrefix)
import Data.Map (fromList)
import Data.Maybe (catMaybes)
import HaSqlDB
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (readFile, writeFile)

serializeDatabase :: Database -> String
serializeDatabase (Database dbMap) =
  Data.Map.foldrWithKey (\name table acc -> acc ++ serializeTable name table) "" dbMap

serializeTable :: Name -> Table -> String
serializeTable name (Table cols records) =
  "Table: " ++ name ++ "\nColumns: " ++ show cols ++ "\nRecords:\n" ++ unlines (map show (Data.Map.elems records))

saveDatabase :: Database -> String -> IO ()
saveDatabase db dbName = do
  let dir = "HaSQL/databases"
  createDirectoryIfMissing True dir
  let filePath = dir </> (dbName ++ ".txt")
  writeFile filePath (serializeDatabase db)

parseDatabase :: String -> Database
parseDatabase str =
  let tables = parseTables str
   in Database $ Data.Map.fromList tables

loadDatabase :: String -> IO Database
loadDatabase dbName = do
  let filePath = "HaSQL/databases" </> (dbName ++ ".txt")
  contents <- readFile filePath
  return $ parseDatabase contents

parseTables :: String -> [(Name, Table)]
parseTables str =
  let tableStrs = splitOn "\n\n" str -- Assuming each table is separated by two newlines
   in mapMaybe parseTable tableStrs

parseTable :: String -> Maybe (Name, Table)
parseTable str = do
  let lines = filter (not . null) $ splitOn "\n" str
  name <- parseTableName $ head lines
  let cols = parseColumns $ lines !! 1
  let records = map parseRecord $ drop 2 lines
  return (name, Table cols records)

parseTableName :: String -> Maybe Name
parseTableName = stripPrefix "Table: "

parseColumns :: String -> [Column]
parseColumns str =
  maybe [] read (stripPrefix "Columns: " str)

parseRecord :: String -> Record
parseRecord str =
  fromList $ read str

splitOn :: String -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c `elem` delimiter = [] : l
      | otherwise = (c : x) : xs
