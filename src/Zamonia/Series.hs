{-# LANGUAGE OverloadedStrings #-}

module Zamonia.Series where

import           Control.Monad          (mzero)
import           Data.Aeson             hiding (Series)
import qualified Data.Csv               as C
import qualified Data.Text              as T
import           Database.SQLite.Simple
import           Text.Printf
import           Text.Replace
import           Zamonia.Work


data Series = Series -- ^ Structure representing a series
    { _sid            :: Int    -- ^ ID
    , _stitle         :: T.Text -- ^ Title
    , _soriginalTitle :: T.Text -- ^ Original Title
    , _sdirector      :: T.Text -- ^ Director
    , _syear          :: T.Text -- ^ Year of release
    , _epNumber       :: T.Text -- ^ Number of episodes
    , _seNumber       :: T.Text -- ^ Number of seasons
    , _spossession    :: T.Text -- ^ Yes/No, Physical/Virtual (for example)
    , _swatched       :: T.Text -- ^ Yes/No
    } deriving Eq

-- | Instance to allow parsing JSON for Series
instance FromJSON Series where
    parseJSON (Object v) = Series <$>
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

-- | Instance to allow transforming a Series to a JSON entry
instance ToJSON Series where
    toJSON (Series i title' originalTitle director year epNumber seNumber possession watched)
      = object ["id" .= i,
                "title" .= title',
                "originalTitle" .= originalTitle,
                "director" .= director,
                "year" .= year,
                "epNumber" .= epNumber,
                "seNumber" .= seNumber,
                "possession" .= possession,
                "watched" .= watched]

-- | Instance to allow parsing CSV for Series
instance C.FromRecord Series where
    parseRecord v
        | length v >= 9 = Series <$> v C..! 0 <*> v C..! 1 <*> v C..! 2 <*> v C..! 3
                                    <*> v C..! 4 <*> v C..! 5 <*> v C..! 6 <*> v C..! 7 <*> v C..! 8
        | otherwise = mzero

-- | Instance to allow transforming a Series to a CSV line
instance C.ToRecord Series where
    toRecord (Series i t o d y en sn p w) = C.record [C.toField i, C.toField t,
                          C.toField o, C.toField d, C.toField y, C.toField en,
                          C.toField sn, C.toField p, C.toField w]

-- | Instance to reading a row as a Series
instance FromRow Series where
  fromRow = Series <$> field <*> field <*> field <*> field
             <*> field <*> field <*> field <*> field <*> field

-- | Instance to transform a Series into a row
instance ToRow Series where
    toRow (Series i t o d y e s p w) = toRow (i, t, o, d, y, e, s, p, w)

-- | Better Show instance => Pretty print of a Series
instance Show Series where
    show (Series _ t o d y e s p w) = printf "Title: %s\nOriginal Title: %s\nDirector: %s\n\
    \Year of release: %s\nNumber of episodes: %s\nNumber of seasons: %s\nPossession: %s\n\
    \Watched: %s" t o d y e s p w

instance Work Series where
    new = Series { _sid = 0
                 , _stitle = T.empty
                 , _soriginalTitle = T.empty
                 , _sdirector = T.empty
                 , _syear = T.empty
                 , _epNumber = T.empty
                 , _seNumber = T.empty
                 , _spossession = T.empty
                 , _swatched = T.empty
                 }
    title = _stitle
    status = _swatched
    id_ = _sid
    addWork conn = execute conn "INSERT OR REPLACE INTO Series VALUES\
                                \ (?,?,?,?,?,?,?,?,?)"
    cmpWork (Series i1 t1 o1 d1 y1 e1 s1 p1 w1) (Series i2 t2 o2 d2 y2 e2 s2 p2 w2) =
        Series
            { _sid = if i1 == -1 then i2 else i1
            , _stitle = compareFields t1 t2
            , _soriginalTitle = compareFields o1 o2
            , _sdirector = compareFields d1 d2
            , _syear = compareFields y1 y2
            , _epNumber = compareFields e1 e2
            , _seNumber = compareFields s1 s2
            , _spossession = compareFields p1 p2
            , _swatched = compareFields w1 w2
            }
    queryAll conn = query_ conn "SELECT * FROM Series"
    replaceList (Series i t o d y e s p w) =  [ Replace "%index%" (T.pack $ show i)
                                             , Replace "%title%" t
                                             , Replace "%originalTitle%" o
                                             , Replace "%director%" d
                                             , Replace "%year%" y
                                             , Replace "%episodeNumber%" e
                                             , Replace "%seasonNumber%" s
                                             , Replace "%possession%" p
                                             , Replace "%watched%" w]

-- | Return a list of all series, sorted the way asked
listSeries :: Sort -> Connection -> IO [(Int, T.Text, T.Text)]
listSeries s conn = query_ conn sql
    where
        sql :: Query
        sql = case s of
                Names -> "SELECT IdS, Done, Title FROM Series ORDER BY Title"
                Done -> "SELECT IdS, Done, Title FROM Series ORDER BY Done"
                Ids -> "SELECT IdS, Done, Title FROM Series"

-- | Delete all rows from Series table
purgeSeries :: Connection -> IO ()
purgeSeries conn = execute_ conn "DELETE FROM Series"
