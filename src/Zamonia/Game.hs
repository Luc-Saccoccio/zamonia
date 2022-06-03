{-# LANGUAGE OverloadedStrings #-}
module Zamonia.Game
    where

import           Control.Monad            (mzero, (>=>))
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as BS
import qualified Data.Csv                 as C
import           Data.Vector              (Vector)
import           Database.SQLite.Simple
import           Text.Printf
import           Text.Replace
import           Zamonia.Work

data Game = Game {}

data GamesCommand =
            GAdd Game
             | GDelete Int
             | GPrint Int
             | GModify Int Game
             | GSearch String String
             | GImportCSV FilePath
             | GImportJSON FilePath
             | GExportJSON FilePath
             | GExportCSV FilePath
             | GExportFormatted FilePath FilePath
             | GList Sort
             | GPurge
