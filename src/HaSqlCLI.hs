module HaSqlCLI where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import HaSqlDB
import HaSqlParser
import HaSqlSyntax
import System.IO (isEOF)

-- Mock database for demonstration purposes
initialDatabase :: Database
initialDatabase = Map.empty

-- Main function for the CLI
main :: IO ()
main = cliLoop initialDatabase

-- The CLI loop
cliLoop :: Database -> IO ()
cliLoop db = do
  putStr "> "
  str <- getLine
  case parseDSL str of
    Left err -> print err
    Right sqlObj ->
      let (s, a) = runState (eval sqlObj) db
       in do
            putStrLn s
            cliLoop a
