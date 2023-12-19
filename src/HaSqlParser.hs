module HaSqlParser where

import Data.Char (isAlphaNum)
import Data.List (intercalate)
import HaSqlSyntax
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as Token



