module HaSqlCLI where

import Control.Monad.State
import Data.List (deleteBy)
import Data.Map (Map)
import Data.Map qualified as Map
import HaSqlDB
import HaSqlFiles
import HaSqlMainParser
import HaSqlOpsComParser
import HaSqlSyntax
import System.IO (isEOF)

-- Main function for the CLI
main :: IO ()
main = cliLoop (DBLoad Nothing Nothing)

-- The client loop, runs HaSqlDB
cliLoop :: DBLoad -> IO ()
cliLoop (DBLoad (Just db) (Just s)) = do
  putStr (s ++ "> ")
  str <- getLine
  case parseOperationalCommand str of
    Right (SCRIPT s2) -> do
      db2 <- runScript db s2
      cliLoop (DBLoad (Just db2) (Just s))
    Right QUIT -> return ()
    Right (NEW s2) -> cliLoop (DBLoad (Just $ DB Map.empty) (Just s2))
    Right SAVE -> saveDatabase (DBLoad (Just db) (Just s)) >> cliLoop (DBLoad (Just db) (Just s))
    Right (SAVEAS s2) -> saveDatabase (DBLoad (Just db) (Just s2)) >> cliLoop (DBLoad (Just db) (Just s2))
    Right (LOAD s2) -> do
      ld <- loadDatabase s2
      cliLoop ld
    Right (DELETEDB s2) -> deleteDatabase s2 >> cliLoop (DBLoad (Just db) (Just s))
    Left _ ->
      case mainParse str of
        Left err -> print err >> cliLoop (DBLoad (Just db) (Just s))
        Right sqlObj ->
          let (s3, a) = runState (eval sqlObj) db
           in do
                putStrLn s3
                cliLoop (DBLoad (Just a) (Just s))
cliLoop mtl = do
  putStr "HaSQL> "
  str <- getLine
  case parseOperationalCommand str of
    Right QUIT -> return ()
    Right (NEW s) -> cliLoop (DBLoad (Just $ DB Map.empty) (Just s))
    Right (LOAD s) -> do
      ld <- loadDatabase s
      cliLoop ld
    Right (DELETEDB s2) -> deleteDatabase s2 >> cliLoop (DBLoad Nothing Nothing)
    Left err -> print err >> cliLoop mtl
    _ -> putStr "No Database Loaded\n" >> cliLoop mtl
