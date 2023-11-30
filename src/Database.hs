module Database where

import CommandLine
import Data.Map
import State

type Column = String

type Name = String

data Database = DB Name [Table]

data Table = Table Name [Column] [Record]

type Record = Map Column Value

displayTable :: Table -> IO ()
displayTable = undefined

select :: ColumnObj -> String -> Clause -> State Database Table
select = undefined

create :: String -> [(String, Value)] -> State Database (Maybe String)
create = undefined

insert :: String -> [(String, Value)] -> State Database (Maybe String)
insert = undefined

update :: String -> [(String, Value)] -> Clause -> State Database (Maybe String)
update = undefined

delete :: String -> Clause -> State Database (Maybe String)
delete = undefined

eval :: SQLObj -> State Database String
eval = undefined

-- should combine state and io monads w/ monad transformers

createDB :: String -> Database
createDB = undefined