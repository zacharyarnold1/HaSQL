module HaSqlFiles where

import Control.Applicative (Alternative (..))
import HaSqlDB
import HaSqlSyntax

saveDatabase :: Database -> String -> ()
saveDatabase db s = undefined

loadDatabase :: String -> Database
loadDatabase = undefined

formatDatabase :: Database -> String
formatDatabase = undefined

parseDatabase :: parser Database
parseDatabase = undefined

parseTable :: parser Table
parseTable = undefined

parseRecord :: parser Record
parseRecord = undefined
