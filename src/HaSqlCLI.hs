module HaSqlCLI where

import Data.List qualified as List
import Data.Maybe
import HaSqlDB
import HaSqlFiles
import HaSqlParser
import HaSqlSyntax
import Parser

data Stepper = Stepper
  { db :: Maybe Database,
    history :: Maybe Stepper,
    dbName :: Maybe String
  }

initialStepper :: Stepper
initialStepper = undefined

cli :: IO ()
cli = go initialStepper
  where
    go :: Stepper -> IO ()
    go ss = do
      prompt ss
      putStr (fromMaybe "HaSQL" (dbName ss) ++ "> ")
      str <- getLine
      case List.uncons (words str) of
        Just ("[SAVE]", _) -> undefined
        Just ("[QUIT]", _) -> return ()
        Just ("[LOAD]", _) -> undefined
        _ -> case db ss of
          Nothing -> putStr "no database selected"
          Just database ->
            case doParse parseInput str of
              Just (SELECT s f w, _) -> select s f w
              Just (CREATE n cs, _) -> create n cs
              Just (INSERT n cs, _) -> insert n cs
              Just (UPDATE n s w, _) -> update n s w
              Just (DELETE n w, _) -> delete n w
              _ -> undefined
    prompt :: Stepper -> IO ()
    prompt = undefined