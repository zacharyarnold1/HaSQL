module HaSqlFiles where

import Control.Exception.Base
import HaSqlSyntax
import LoadParser
import System.IO.Error (isDoesNotExistError)

saveDatabase :: DBLoad -> IO ()
saveDatabase (DBLoad (Just db) (Just dbName)) = do
  let dbString = databaseToString db
  let fileName = "databases/" ++ dbName ++ ".txt"
  writeFile fileName dbString
saveDatabase _ = putStrLn "Error: No database or database name provided"

loadDatabase :: String -> IO DBLoad
loadDatabase dbName = do
  let fileName = "databases/" ++ dbName ++ ".txt"
  fileContents <- try (readFile fileName) :: IO (Either IOError String)
  case fileContents of
    Left ex ->
      if isDoesNotExistError ex
        then return $ DBLoad Nothing Nothing
        else ioError ex
    Right contents ->
      case parseDatabaseString contents of
        Left err ->
          return $ DBLoad Nothing Nothing
        Right db ->
          return $ DBLoad (Just db) (Just dbName)
