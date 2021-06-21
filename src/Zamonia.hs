{-# LANGUAGE OverloadedStrings #-}
module Zamonia
    ( FilmsCommand(..)
    , SeriesCommand(..)
    , Film(..)
    , Serie(..)
    , Sort(..)
    , (<~>)
    , localLocation
    , connection
    , listFilms
    , listSeries
    , addWork
    , delFilm
    , delSerie
    , printFilm
    , printSerie
    , modWork
    , importSeriesCSV
    , importFilmsCSV
    , importSeriesJSON
    , importFilmsJSON
    , exportSeriesCSV
    , exportSeriesJSON
    , exportFilmsCSV
    , exportFilmsJSON
    , purgeFilms
    , purgeSeries)
    where

import           Control.Exception        (bracket)
import           Control.Monad            (mzero, (<=<), (>=>))
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as BS
import qualified Data.Csv                 as C
import           Data.Functor             ((<&>))
import           Data.Sort
import           Data.Vector              (Vector)
import           Database.SQLite.Simple
import           System.Environment       (getEnv)
import           Text.Printf

localLocation :: IO FilePath
localLocation = getEnv "HOME" <&> (++ "/.local/share/zamonia")

databaseLocation :: IO FilePath
databaseLocation = localLocation <&> (++ "/zamonia.db")

class Work a where
    title :: a -> String -- ^ Title of the work
    id_ :: a -> Int -- ^ ID of the work
    modWork :: Connection -> Int -> a -> IO () -- ^ Modifying the informations of a work
    addWork :: Connection -> a -> IO () -- ^ Adding a work
    cmpWork :: a -> a -> a -- ^ Comparing two works

data Serie = Serie -- ^ Structure representing a serie
    { sid            :: Int    -- ^ ID
    , stitle         :: String -- ^ Title
    , soriginalTitle :: String -- ^ Original Title
    , sdirector      :: String -- ^ Director
    , syear          :: String -- ^ Year of release
    , epNumber       :: String -- ^ Number of episodes
    , seNumber       :: String -- ^ Number of seasons
    , spossession    :: String -- ^ Yes/No, Physical/Virtual (for example)
    , swatched       :: String -- ^ Yes/No
    } deriving Eq

instance FromJSON Serie where
    parseJSON (Object v) = Serie <$>
                    v .: "id" <*>
                    v .: "title" <*>
                    v .: "originalTitle" <*>
                    v .: "director" <*>
                    v .: "year" <*>
                    v .: "epNumber" <*>
                    v .: "seNumber" <*>
                    v .: "possession" <*>
                    v .: "watched"
    parseJSON _ = mzero

instance ToJSON Serie where
    toJSON (Serie i title originalTitle director year epNumber seNumber possession watched)
      = object ["id" .= i,
                "title" .= title,
                "originalTitle" .= originalTitle,
                "director" .= director,
                "year" .= year,
                "epNumber" .= epNumber,
                "seNumber" .= seNumber,
                "possession" .= possession,
                "watched" .= watched]

instance C.FromRecord Serie where
    parseRecord v
        | length v >= 8 = Serie <$> v C..! 0 <*> v C..! 1 <*> v C..! 2 <*> v C..! 3
                                    <*> v C..! 4 <*> v C..! 5 <*> v C..! 6 <*> v C..! 7 <*> v C..! 8
        | otherwise = mzero

instance C.ToRecord Serie where
    toRecord (Serie i t o d y en sn p w) = C.record [C.toField i, C.toField t,
                          C.toField o, C.toField d, C.toField y, C.toField en,
                          C.toField sn, C.toField p, C.toField w]

instance FromRow Serie where
  fromRow = Serie <$> field <*> field <*> field <*> field
             <*> field <*> field <*> field <*> field <*> field

instance ToRow Serie where
    toRow (Serie i t o d y e s p w) = toRow (i, t, o, d, y, e, s, p, w)

instance Show Serie where
    show (Serie _ t o d y e s p w) = printf "\ESC[1;37mTitle:\ESC[m %s\n\ESC[1;37mOriginal Title:\ESC[m %s\n\ESC[1;37mDirector:\ESC[m %s\n\
    \\ESC[1;37mYear or release:\ESC[m %s\n\ESC[1;37mNumber of episodes:\ESC[m %s\n\ESC[1;37mNumber of seasons:\ESC[m %s\n\ESC[1;37mPossession:\ESC[m %s\n\
    \\ESC[1;37mWatched:\ESC[m %s" t o d y e s p w

instance Work Serie where
    title = stitle
    id_ = sid
    addWork conn = execute conn "INSERT OR REPLACE INTO Series VALUES\
                                \ (?,?,?,?,?,?,?,?,?)"
    cmpWork (Serie i1 t1 o1 d1 y1 e1 s1 p1 w1) (Serie i2 t2 o2 d2 y2 e2 s2 p2 w2) =
        Serie
            { sid = if i2 == -1 then i1 else i2
            , stitle = compareFields t1 t2
            , soriginalTitle = compareFields o1 o2
            , sdirector = compareFields d1 d2
            , syear = compareFields y1 y2
            , epNumber = compareFields e1 e2
            , seNumber = compareFields s1 s2
            , spossession = compareFields p1 p2
            , swatched = compareFields w1 w2
            }
    modWork conn n s = (addWork conn . cmpWork s . head) =<<
        (queryNamed conn "SELECT * FROM Series WHERE IdS = :id" [":id" := n] :: IO [Serie])

data Film = Film -- ^ Structure representing a film
    { fid            :: Int    -- ^ ID
    , ftitle         :: String -- ^ Title
    , foriginalTitle :: String -- ^ Original Title
    , fdirector      :: String -- ^ Director
    , fyear          :: String -- ^ Year of release
    , fpossession    :: String -- ^ Yes/No, Physical/Virtual (for example)
    , fwatched       :: String -- ^ Yes/No
    } deriving Eq

instance FromJSON Film where
    parseJSON (Object v) = Film <$>
        v .: "id" <*>
        v .: "title" <*>
        v .: "originalTitle" <*>
        v .: "director" <*>
        v .: "year" <*>
        v .: "possession" <*>
        v .: "watched"
    parseJSON _ = mzero

instance ToJSON Film where
    toJSON (Film i title originalTitle director year possession watched)
      = object ["id" .= i,
                "title" .= title,
                "originalTitle" .= originalTitle,
                "director" .= director,
                "year" .= year,
                "possession" .= possession,
                "watched" .= watched]

instance C.FromRecord Film where
    parseRecord v
        | length v >= 6 = Film <$> v C..! 0 <*> v C..! 1 <*> v C..! 2 <*> v C..! 3
                                    <*> v C..! 4 <*> v C..! 5 <*> v C..! 6
        | otherwise = mzero

instance C.ToRecord Film where
    toRecord (Film i t o d y p w) = C.record [C.toField i, C.toField t,
        C.toField o, C.toField d, C.toField y, C.toField p, C.toField w]

instance FromRow Film where
  fromRow = Film <$> field <*> field <*> field <*> field
             <*> field <*> field <*> field

instance ToRow Film where
    toRow (Film i t o d y p w) = toRow (i, t, o, d, y, p, w)

instance Show Film where
    show (Film _ t o d y p w) = printf "\ESC[1;37mTitle:\ESC[m %s\n\ESC[1;37mOriginal Title:\ESC[m %s\n\ESC[1;37mDirector:\ESC[m %s\n\
    \\ESC[1;37mYear or release:\ESC[m %s\n\ESC[1;37mPossession:\ESC[m %s\n\
    \\ESC[1;37mWatched:\ESC[m %s" t o d y p w

instance Work Film where
    title = ftitle
    id_ = fid
    addWork conn = execute conn "INSERT OR REPLACE INTO Films VALUES\
                                \ (?,?,?,?,?,?,?)"
    cmpWork (Film i1 t1 o1 d1 y1 p1 w1) (Film i2 t2 o2 d2 y2 p2 w2) =
        Film
            { fid = if i2 == -1 then i1 else i2
            , ftitle = compareFields t1 t2
            , foriginalTitle = compareFields o1 o2
            , fdirector = compareFields d1 d2
            , fyear = compareFields y1 y2
            , fpossession = compareFields p1 p2
            , fwatched = compareFields w1 w2
            }
    modWork conn n f = (addWork conn . cmpWork f . head) =<<
        (queryNamed conn "SELECT * FROM Films WHERE IdF = :id" [":id" := n] :: IO [Film])

compareFields :: String -> String -> String
compareFields s1 s2 = if null s2 then s1 else s2

delFilm :: Connection -> Int -> IO ()
delFilm conn n = execute conn "DELETE FROM Films WHERE IdF = ?" (Only n)

delSerie :: Connection -> Int -> IO ()
delSerie conn n = execute conn "DELETE FROM Series WHERE IdS = ?" (Only n)

listFilms :: Sort -> Connection -> IO [(Int, String, String)]
listFilms s conn = query_ conn sql
    where
        sql = case s of
                Names -> "SELECT IdF, Watched, Title FROM Films ORDER BY Title"
                Watched -> "SELECT IdF, Watched, Title FROM Films ORDER BY Watched"
                Ids -> "SELECT IdF, Watched, Title FROM Films"

listSeries :: Sort -> Connection -> IO [(Int, String, String)]
listSeries s conn = query_ conn sql
    where
        sql = case s of
                Names -> "SELECT IdS, Watched, Title FROM Series ORDER BY Title"
                Watched -> "SELECT IdS, Watched, Title FROM Series ORDER BY Watched"
                Ids -> "SELECT IdS, Watched, Title FROM Series"

printFilm :: Connection -> Int -> IO ()
printFilm conn n =
    fetchFilms >>= print . head
  where
    fetchFilms :: IO [Film]
    fetchFilms = queryNamed conn sql [":id" := n]
    sql = "SELECT * FROM Films WHERE IdF = :id"

printSerie :: Connection -> Int -> IO ()
printSerie conn n =
    fetchSeries >>= print . head
        where
    fetchSeries :: IO [Serie]
    fetchSeries = queryNamed conn sql [":id" := n]
    sql = "SELECT * FROM Series WHERE IdS = :id"

orPrint :: Either String a -> (a -> IO()) -> IO ()
orPrint = flip (either putStrLn)

-- | Trying to open a database and returning the Connection.
-- It takes a function. If the function fails, connection closes the connection to the database.
connection :: (Connection -> IO c) -> IO c
connection = bracket (open =<< databaseLocation) close

-- | Read the specified file, try to decode it. If it fails, print the error.
-- If it didn't fail, add each work to the database.
importFilmsJSON :: Connection -> FilePath -> IO ()
importFilmsJSON conn = BS.readFile >=> \j -> orPrint (eitherDecode j :: Either String [Film])
                        $ mapM_ (addWork conn)

-- | Read the specified file, try to decode it. If it fails, print the error.
-- If it didn't fail, add each work to the database.
importSeriesJSON :: Connection -> FilePath -> IO ()
importSeriesJSON conn = BS.readFile >=> \j -> orPrint (eitherDecode j :: Either String [Serie])
                        $ mapM_ (addWork conn)

-- | Read the specified file, try to decode it. If it fails, print the error.
-- If it didn't fail, add each work to the database.
importFilmsCSV :: Connection -> FilePath -> IO ()
importFilmsCSV conn = BS.readFile >=> \c -> orPrint (C.decode C.HasHeader c :: Either String (Vector Film))
                        $ mapM_ (addWork conn)

-- | Read the specified file, try to decode it. If it fails, print the error.
-- If it didn't fail, add each work to the database.
importSeriesCSV :: Connection -> FilePath -> IO ()
importSeriesCSV conn = BS.readFile >=> \c -> orPrint (C.decode C.HasHeader c :: Either String (Vector Serie))
                        $ mapM_ (addWork conn)

-- | Query the films, encode them and write them to the specified file.
exportFilmsJSON :: Connection -> FilePath -> IO ()
exportFilmsJSON conn file = BS.writeFile file . encodePretty =<< films
    where
        films = query_ conn "SELECT * FROM Films" :: IO [Film]

-- | Query the series, encode them and write them to the specified file.
exportSeriesJSON :: Connection -> FilePath -> IO ()
exportSeriesJSON conn file = BS.writeFile file . encodePretty =<< series
    where
        series = query_ conn "SELECT * FROM Series" :: IO [Serie]

-- | Query the films, encode them and write them to the specified file.
exportFilmsCSV :: Connection -> FilePath -> IO ()
exportFilmsCSV conn file = BS.writeFile file . C.encode =<< films
    where
        films = query_ conn "SELECT * FROM Films" :: IO [Film]

-- | Query the series, encode them and write them to the specified file.
exportSeriesCSV :: Connection -> FilePath -> IO ()
exportSeriesCSV conn file = BS.writeFile file . C.encode =<< series
    where
        series = query_ conn "SELECT * FROM Series" :: IO [Serie]

purgeFilms :: Connection -> IO ()
purgeFilms conn = execute_ conn "DELETE FROM Films"

purgeSeries :: Connection -> IO ()
purgeSeries conn = execute_ conn "DELETE FROM Series"

-- | Types of sort
data Sort = Ids
          | Names
          | Watched

-- | Select sort
(<~>) :: Sort -> Sort -> Sort
(<~>) _ Names = Names
(<~>) _ Watched = Watched
(<~>) a Ids = a

-- | Commands related to films
data FilmsCommand =
            FAdd Film
             | FDelete Int
             | FPrint Int
             | FModify Int Film
             | FSearch String String
             | FImportCSV FilePath
             | FImportJSON FilePath
             | FExportJSON FilePath
             | FExportCSV FilePath
             | FList Sort
             | FPurge

-- | Commands related to series
data SeriesCommand =
            SAdd Serie
             | SDelete Int
             | SPrint Int
             | SModify Int Serie
             | SSearch String String
             | SImportCSV FilePath
             | SImportJSON FilePath
             | SExportJSON FilePath
             | SExportCSV FilePath
             | SList Sort
             | SPurge
