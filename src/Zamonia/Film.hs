{-# LANGUAGE OverloadedStrings #-}
module Zamonia.Film
    where

import           Control.Monad          (mzero)
import           Data.Aeson
import           Data.Csv               ((.!))
import qualified Data.Csv               as C hiding ((.!))
import qualified Data.Text              as T
import           Database.SQLite.Simple
import           Text.Printf
import           Text.Replace
import           Zamonia.Work

-- | Structure representing a film
data Film = Film
    { _fid            :: Int    -- ^ ID
    , _ftitle         :: T.Text -- ^ Title
    , _foriginalTitle :: T.Text -- ^ Original Title
    , _fdirector      :: T.Text -- ^ Director
    , _fyear          :: T.Text -- ^ Year of release
    , _fpossession    :: T.Text -- ^ Yes/No, Physical/Virtual (for example)
    , _fwatched       :: T.Text -- ^ Yes/No
    } deriving Eq

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
    toJSON (Film i title' originalTitle director year possession watched)
      = object ["id" .= i,
                "title" .= title',
                "originalTitle" .= originalTitle,
                "director" .= director,
                "year" .= year,
                "possession" .= possession,
                "watched" .= watched]

-- | Instance to allow parsing CSV for Film
instance C.FromRecord Film where
    parseRecord v
        | length v >= 7 = Film <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3
                                    <*> v .! 4 <*> v .! 5 <*> v .! 6
        | otherwise = mzero -- Fail if the number of field if too low

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
    show (Film _ t o d y p w) = printf "Title: %s\nOriginal Title: %s\nDirector: %s\n\
    \Year of release: %s\nPossession: %s\n\
    \Watched: %s" t o d y p w

instance Work Film where
    new = Film { _fid = -1
               , _ftitle = T.empty
               , _foriginalTitle = T.empty
               , _fdirector = T.empty
               , _fyear = T.empty
               , _fpossession = T.empty
               , _fwatched = T.empty
               }
    title = _ftitle
    status = _fwatched
    id_ = _fid
    addWork conn = execute conn "INSERT OR REPLACE INTO Films VALUES\
                                \ (?,?,?,?,?,?,?)"
    cmpWork (Film i1 t1 o1 d1 y1 p1 w1) (Film i2 t2 o2 d2 y2 p2 w2) =
        Film
            { _fid = if i2 == -1 then i1 else i2
            , _ftitle = compareFields t1 t2
            , _foriginalTitle = compareFields o1 o2
            , _fdirector = compareFields d1 d2
            , _fyear = compareFields y1 y2
            , _fpossession = compareFields p1 p2
            , _fwatched = compareFields w1 w2
            }
    queryAll conn = query_ conn "SELECT * FROM Films"
    replaceList (Film i t o d y p w) = [ Replace "%index%" (T.pack $ show i)
                                       , Replace "%title%" t
                                       , Replace "%originalTitle%" o
                                       , Replace "%director%" d
                                       , Replace "%year%" y
                                       , Replace "%possession%" p
                                       , Replace "%watched%" w]

-- | Return a list of all films, sorted the way asked
listFilms :: Sort -> Connection -> IO [(Int, T.Text, T.Text)]
listFilms s conn = query_ conn sql
    where
        sql :: Query
        sql = case s of
                Names -> "SELECT IdF, Done, Title FROM Films ORDER BY Title" -- Sorting by name
                Done  -> "SELECT IdF, Done, Title FROM Films ORDER BY Done" -- Sorting by watching state
                Ids   -> "SELECT IdF, Done, Title FROM Films" -- Default sort => by index

-- | Delete all entries in Films table
purgeFilms :: Connection -> IO ()
purgeFilms conn = execute_ conn "DELETE FROM Films"
