module HaSqlFiles where

import Control.Exception.Base
import HaSqlSyntax
import LoadParser
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)

-- saves a database by writing its contents to a textfile
saveDatabase :: DBLoad -> IO ()
saveDatabase (DBLoad (Just db) (Just dbName)) = do
  let dbString = databaseToString db
  let fileName = "databases/" ++ dbName ++ ".txt"
  writeFile fileName dbString
saveDatabase _ = putStrLn "Error: No database or database name provided"

-- deletes a database of specified name
deleteDatabase :: String -> IO ()
deleteDatabase dbName = do
  let fileName = "databases/" ++ dbName ++ ".txt"
  result <- try (removeFile fileName) :: IO (Either IOError ())
  case result of
    Left ex ->
      if isDoesNotExistError ex
        then putStrLn "DB Dosen't Exist"
        else ioError ex
    Right _ -> putStrLn "DB Deleted Successfully"

-- removes a file of specified name (i.e. deletes contents of a database in memory)
-- removeFile fileName

-- loads a database specified by its name by deserializing the contents of the textfile, returns a DBLoad containing contents of the saved database
loadDatabase :: String -> IO DBLoad
loadDatabase dbName = do
  let fileName = "databases/" ++ dbName ++ ".txt"
  fileContents <- try (readFile fileName) :: IO (Either IOError String)
  case fileContents of
    Left ex ->
      if isDoesNotExistError ex
        then do
          putStrLn "DB Doesn't Exist"
          return $ DBLoad Nothing Nothing
        else ioError ex
    Right contents ->
      case parseDatabaseString contents of
        Left err ->
          return $ DBLoad Nothing Nothing
        Right db ->
          return $ DBLoad (Just db) (Just dbName)
