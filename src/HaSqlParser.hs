module HaSqlParser where

import Control.Applicative (Alternative (..))
import HaSqlSyntax
import Parser
import Test.HUnit (Test)

reserved :: Char -> Bool
reserved c = c `elem` ['[', ']']

text :: Parser String
text = some (satisfy (not . reserved))

parseInput :: Parser SQLObj
parseInput = parseSelect <|> parseCreate <|> parseInsert <|> parseUpdate <|> parseDelete

parseSelect :: Parser SQLObj
parseSelect = SELECT <$> parseColumn <*> text <*> parseClause

parseCreate :: Parser SQLObj
parseCreate = CREATE <$> text <*> parseRecord

parseInsert :: Parser SQLObj
parseInsert = INSERT <$> text <*> parseRecord

parseUpdate :: Parser SQLObj
parseUpdate = UPDATE <$> text <*> parseRecord <*> parseClause

parseDelete :: Parser SQLObj
parseDelete = DELETE <$> text <*> parseClause

parseClause :: Parser Clause
parseClause = undefined

parseColumn :: Parser ColumnObj
parseColumn = undefined

parseRecord :: Parser [(String, Value)]
parseRecord = char '(' *> text <* char ')'

test_parseRecord :: Test
test_parseRecord = TestList [
    parse parseEntry "({name, 'Kailash'}, {}, {})"
]


