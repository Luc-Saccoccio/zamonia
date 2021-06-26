{-# LANGUAGE OverloadedStrings #-}
module Zamonia
    ( FilmsCommand(..)
    , SeriesCommand(..)
    , Film(..)
    , Series(..)
    , Sort(..)
    , (<~>)
    , localLocation
    , connection
    , listFilms
    , listSeries
    , addWork
    , delFilm
    , delSeries
    , printFilm
    , printSeries
    , modWork
    , importSeriesCSV
    , importFilmsCSV
    , importSeriesJSON
    , importFilmsJSON
    , exportSeriesCSV
    , exportSeriesJSON
    , seriesToFullFormatted
    , filmsToFullFormatted
    , exportFilmsCSV
    , exportFilmsJSON
    , purgeFilms
    , purgeSeries)
    where

import           Control.Exception      (bracket)
import           Data.Functor           ((<&>))
import           Database.SQLite.Simple
import           System.Environment     (getEnv)
import           Zamonia.Film
import           Zamonia.Series
import           Zamonia.Work


-- | Location of zamonia "home" for files
localLocation :: IO FilePath
localLocation = getEnv "HOME" <&> (++ "/.local/share/zamonia")

-- | Location of the database
databaseLocation :: IO FilePath
databaseLocation = localLocation <&> (++ "/zamonia.db")

-- | Trying to open a database and returning the Connection.
-- It takes a function. If the function fails, connection closes the connection to the database.
connection :: (Connection -> IO c) -> IO c
connection = bracket (open =<< databaseLocation) (\c -> execute_ c "PRAGMA optimize" >> close c)
