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
