{-# LANGUAGE OverloadedStrings #-}
module Zamonia
    ( FilmsCommand(..)
    , SeriesCommand(..)
    , BooksCommand(..)
    , Film(..)
    , Series(..)
    , Book(..)
    , Sort(..)
    , (<~>)
    , localLocation
    , connection

    , listFilms
    , listSeries
    , listBooks

    , delFilm
    , delSeries
    , delBook

    , printFilm
    , printSeries
    , printBook

    , addWork
    , modWork

    , importSeriesCSV
    , importFilmsCSV
    , importBooksCSV

    , importSeriesJSON
    , importFilmsJSON
    , importBooksJSON

    , exportSeriesCSV
    , exportFilmsCSV
    , exportBooksCSV

    , exportSeriesJSON
    , exportFilmsJSON
    , exportBooksJSON

    , seriesToFullFormatted
    , filmsToFullFormatted
    , booksToFullFormatted

    , purgeFilms
    , purgeSeries
    , purgeBooks)
    where

import           Control.Exception      (bracket)
import           Data.Functor           ((<&>))
import           Database.SQLite.Simple
import           System.Environment     (getEnv)
import           Zamonia.Film
import           Zamonia.Series
import           Zamonia.Book
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
