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
  , booksLocation
  , seriesLocation
  , filmsLocation
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
  , importCSV
  , importJSON
  , exportCSV
  , exportJSON
  , allToFullFormatted
  , purgeFilms
  , purgeSeries
  , purgeBooks
  ) where

import           Control.Exception      (bracket)
import           Data.Functor           ((<&>))
import           Data.Maybe             (fromMaybe)
import           Database.SQLite.Simple
import           System.Environment     (getEnv, lookupEnv)
import           Zamonia.Book
import           Zamonia.Film
import           Zamonia.Series
import           Zamonia.Work


-- | Location of zamonia "home" for files
localLocation :: IO FilePath
localLocation = maybe (getEnv "HOME" <&> (++ "/.local/share/zamonia")) (return . (++ "/zamonia")) =<< lookupEnv "XDG_DATA_HOME"

-- | Location of Books commentary
booksLocation :: IO FilePath
booksLocation = localLocation <&> (++ "/Books/")

-- | Location of Series commentary
seriesLocation :: IO FilePath
seriesLocation = localLocation <&> (++ "/Series/")

-- | Location of Films commentary
filmsLocation :: IO FilePath
filmsLocation = localLocation <&> (++ "/Films/")

-- | Location of the database
databaseLocation :: IO FilePath
databaseLocation = localLocation <&> (++ "/zamonia.db")

-- | The editor to use. Default to "vi"
editor :: IO String
editor = lookupEnv "EDITOR" <&> fromMaybe "vi"

-- | Trying to open a database and returning the Connection.
-- It takes a function. If the function fails, connection closes the connection to the database.
connection :: (Connection -> IO c) -> IO c
connection = bracket (open =<< databaseLocation)
                     (\c -> execute_ c "PRAGMA optimize" >> close c)
