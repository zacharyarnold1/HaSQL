module QueryHandler where

import CommandLine

import Database

classifier :: SQLObj -> ()
classifier (SELECT _ _ _) = undefined -- send this to select function
classifier (CREATE _ _ ) = undefined -- send to create
classifier (INSERT _ _) = undefined -- send to insert
classifier (UPDATE _ _ _) = undefined -- send to update
classifier (DELETE _ _) = undefined -- send to delete



