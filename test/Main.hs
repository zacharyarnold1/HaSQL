module Main where

import DBTest
import HaSqlQC
import HaSqlUnitJoinTests
import ParserTest

main :: IO ()
main = do
  runAllTests
  runParserTests
  runJoinTests
  runDBTests
  return ()