module HaSqlParser where

import Control.Applicative (Alternative (..))
import HaSqlSyntax
import Parser

reserved :: Char -> Bool
reserved c = c `elem` ['[', ']']

text :: Parser String
text = some (satisfy (not . reserved))

parseInput :: Parser SQLObj
parseInput = parseSelect <|> parseCreate <|> parseInsert <|> parseUpdate <|> parseDelete

parseSelect :: Parser SQLObj
parseSelect = SELECT <$> parseColumn <*> text <*> parseClause

parseCreate :: Parser SQLObj
parseCreate = CREATE <$> text <*> parseEntry

parseInsert :: Parser SQLObj
parseInsert = INSERT <$> text <*> parseEntry

parseUpdate :: Parser SQLObj
parseUpdate = UPDATE <$> text <*> parseEntry <*> parseClause

parseDelete :: Parser SQLObj
parseDelete = DELETE <$> text <*> parseClause

parseClause :: Parser Clause
parseClause = undefined

parseColumn :: Parser ColumnObj
parseColumn = undefined

parseEntry :: Parser [(String, Value)]
parseEntry = undefined
