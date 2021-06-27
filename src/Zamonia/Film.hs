{-# LANGUAGE OverloadedStrings #-}
module Zamonia.Film
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

-- | Structure representing a film
data Film = Film 
    { fid            :: Int    -- ^ ID
    , ftitle         :: String -- ^ Title
    , foriginalTitle :: String -- ^ Original Title
    , fdirector      :: String -- ^ Director
    , fyear          :: String -- ^ Year of release
    , fpossession    :: String -- ^ Yes/No, Physical/Virtual (for example)
    , fwatched       :: String -- ^ Yes/No
    } deriving Eq

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
             | FExportFormatted FilePath FilePath
             | FList Sort
             | FPurge

-- | Instance to allow parsing JSON for Film
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

-- | Instance to allow transforming a Film to a JSON entry
instance ToJSON Film where
    toJSON (Film i title originalTitle director year possession watched)
      = object ["id" .= i,
                "title" .= title,
                "originalTitle" .= originalTitle,
                "director" .= director,
                "year" .= year,
                "possession" .= possession,
                "watched" .= watched]

-- | Instance to allow parsing CSV for Film
instance C.FromRecord Film where
    parseRecord v
        | length v >= 7 = Film <$> v C..! 0 <*> v C..! 1 <*> v C..! 2 <*> v C..! 3
                                    <*> v C..! 4 <*> v C..! 5 <*> v C..! 6
        | otherwise = mzero -- | Fail if the number of field if too low

-- | Instance to allow transforming a Film to a CSV line
instance C.ToRecord Film where
    toRecord (Film i t o d y p w) = C.record [C.toField i, C.toField t,
        C.toField o, C.toField d, C.toField y, C.toField p, C.toField w]

-- | Instance to allow reading a row as a Film
instance FromRow Film where
  fromRow = Film <$> field <*> field <*> field <*> field
             <*> field <*> field <*> field

-- | Instance to transform a film into a row
instance ToRow Film where
    toRow (Film i t o d y p w) = toRow (i, t, o, d, y, p, w)

-- | Better Show instance => Pretty Print of a Film
instance Show Film where
    show (Film _ t o d y p w) = printf "\ESC[1;37mTitle:\ESC[m %s\n\ESC[1;37mOriginal Title:\ESC[m %s\n\ESC[1;37mDirector:\ESC[m %s\n\
    \\ESC[1;37mYear of release:\ESC[m %s\n\ESC[1;37mPossession:\ESC[m %s\n\
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
    replaceList (Film i t o d y p w) = [ Replace "%index%" (show i)
                                       , Replace "%title%" t
                                       , Replace "%originalTitle%" o
                                       , Replace "%director%" d
                                       , Replace "%year%" y
                                       , Replace "%possession%" p
                                       , Replace "%watched%" w]

-- | Delete the film matching the index
delFilm :: Connection -> Int -> IO ()
delFilm conn n = execute conn "DELETE FROM Films WHERE IdF = ?" (Only n)

-- | Return a list of all films, sorted the way asked
listFilms :: Sort -> Connection -> IO [(Int, String, String)]
listFilms s conn = query_ conn sql
    where
        sql :: Query
        sql = case s of
                Names -> "SELECT IdF, Done, Title FROM Films ORDER BY Title" -- Sorting by name
                Done -> "SELECT IdF, Done, Title FROM Films ORDER BY Done" -- Sorting by watching state
                Ids -> "SELECT IdF, Done, Title FROM Films" -- Default sort => by index

-- | Print a film
printFilm :: Connection -> Int -> IO ()
printFilm conn n =
    fetchFilms >>= putStrLn . printEmpty
  where
    fetchFilms :: IO [Film]
    fetchFilms = queryNamed conn sql [":id" := n]
    sql :: Query
    sql = "SELECT * FROM Films WHERE IdF = :id"

-- | Read the specified file, try to decode it. If it fails, print the error.
-- If it didn't fail, add each work to the database.
importFilmsJSON :: Connection -> FilePath -> IO ()
importFilmsJSON conn = BS.readFile >=> \j -> orPrint (eitherDecode j :: Either String [Film])
                        $ mapM_ (addWork conn)

-- | Read the specified file, try to decode it. If it fails, print the error.
-- If it didn't fail, add each work to the database.
importFilmsCSV :: Connection -> FilePath -> IO ()
importFilmsCSV conn = BS.readFile >=> \c -> orPrint (C.decode C.HasHeader c :: Either String (Vector Film))
                        $ mapM_ (addWork conn)

-- | Query the films, encode them and write them to the specified file.
exportFilmsJSON :: Connection -> FilePath -> IO ()
exportFilmsJSON conn file = BS.writeFile file . encodePretty =<< films
    where
        films = query_ conn "SELECT * FROM Films" :: IO [Film]

-- | Query the films, encode them and write them to the specified file.
exportFilmsCSV :: Connection -> FilePath -> IO ()
exportFilmsCSV conn file = BS.writeFile file . C.encode =<< films
    where
        films = query_ conn "SELECT * FROM Films" :: IO [Film]

-- | Delete all entries in Films table
purgeFilms :: Connection -> IO ()
purgeFilms conn = execute_ conn "DELETE FROM Films"

-- | Convert each entry to a formatted string, and concatenate
filmsToFullFormatted :: FilePath -> Connection -> IO String
filmsToFullFormatted file conn = fmap concat . mapM toFormatted =<< films
    where
        toFormatted :: Film -> IO String
        toFormatted = toFullFormatted template -- | Prepare with the template
        template :: IO String
        template = readFile file
        films :: IO [Film]
        films = query_ conn "SELECT * FROM Films"
