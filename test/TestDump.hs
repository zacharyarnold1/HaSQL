module TestDump where

import Control.Monad.State
import Data.Bool (Bool (False))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import HaSqlArbitrary
import HaSqlClauseLogic
import HaSqlDBOps
import HaSqlDBRecordOps
import HaSqlDBSelect
import HaSqlFiles
import HaSqlMainParser
import HaSqlPrettyPrint
import HaSqlSyntax
import Test.QuickCheck

prop_saveLoadRoundTrip :: Database -> Property
prop_saveLoadRoundTrip db = ioProperty $ do
  let dbName = "testDB" -- A temporary database name for testing
      dbLoad = DBLoad (Just db) (Just dbName)
  saveDatabase dbLoad -- Save the database
  dbLoadResult <- loadDatabase dbName -- Load the saved database
  deleteDatabase dbName -- Clean up the test database file
  return $ case dbLoadResult of
    DBLoad (Just loadedDb) _ -> db == loadedDb
    _ -> False

prop_loadNonExistentDatabase :: Property
prop_loadNonExistentDatabase = ioProperty $ do
  let nonExistentDbName = "nonExistentDB"
  dbLoadResult <- loadDatabase nonExistentDbName
  return $ case dbLoadResult of
    DBLoad Nothing _ -> True
    _ -> False

prop_deleteThenLoadDatabase :: Database -> Property
prop_deleteThenLoadDatabase db = ioProperty $ do
  let dbName = "testDBForDelete"
      dbLoad = DBLoad (Just db) (Just dbName)
  saveDatabase dbLoad -- Save the database
  deleteDatabase dbName -- Delete the database
  dbLoadResult <- loadDatabase dbName -- Attempt to load the deleted database
  return $ case dbLoadResult of
    DBLoad Nothing _ -> True
    _ -> False

prop_saveLoadEmptyDatabase :: Property
prop_saveLoadEmptyDatabase = ioProperty $ do
  let emptyDb = DB Map.empty
      dbName = "emptyTestDB"
      dbLoad = DBLoad (Just emptyDb) (Just dbName)
  saveDatabase dbLoad -- Save the empty database
  dbLoadResult <- loadDatabase dbName -- Load the saved database
  deleteDatabase dbName -- Clean up the test database file
  return $ case dbLoadResult of
    DBLoad (Just loadedDb) _ -> emptyDb == loadedDb
    _ -> False
