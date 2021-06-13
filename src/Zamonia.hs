{-# LANGUAGE OverloadedStrings #-}
module Zamonia where

import           Control.Monad      (mzero)
import           Data.Aeson
import qualified Data.Csv           as C
import           Database.SQLite.Simple
import           Data.Sort
import qualified Data.Text          as T
import           Text.Printf

class Work a where
    title :: a -> T.Text
    id_ :: a -> Int
    modWork :: Connection -> a -> IO ()
    addWork :: Connection -> a -> IO ()
    addWork = undefined

data Serie = Serie
    { sid            :: Int -- ID
    , stitle         :: T.Text -- Title
    , soriginalTitle :: T.Text -- Original Title
    , sdirector      :: T.Text -- Director
    , syear          :: T.Text -- Year of release
    , epNumber       :: T.Text -- Number of episodes
    , seNumber       :: T.Text -- Number of seasons
    , spossession    :: T.Text -- Yes/No, Physical/Virtual (for example)
    , swatched       :: T.Text -- Yes/No
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

data Film = Film
    { fid            :: Int -- ID
    , ftitle         :: T.Text -- Title
    , foriginalTitle :: T.Text -- Original Title
    , fdirector      :: T.Text -- Director
    , fyear          :: T.Text -- Year of release
    , fpossession    :: T.Text -- Yes/No, Physical/Virtual (for example)
    , fwatched       :: T.Text -- Yes/No
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

data FilmsCommand = FAdd Film
             | FDelete Int
             | FPrint Int
             | FModify Int Film
             | FSearch String T.Text
             | FImport String
             | FExport String
             | FSort String
             | FList deriving Show

data SeriesCommand = SAdd Serie
             | SDelete Int
             | SPrint Int
             | SModify Int Serie
             | SSearch String T.Text
             | SImport FilePath
             | SExport FilePath
             | SSort String
             | SList deriving Show

delWork :: Connection -> Int -> IO ()
delWork = undefined
listWork :: Connection -> IO [String]
listWork = undefined

orPrint :: Either String a -> (a -> IO()) -> IO ()
orPrint = flip (either putStrLn)

orIndexError :: Maybe a -> (a -> IO ()) -> IO ()
orIndexError s f = case s of
                     Just x  -> f x
                     Nothing -> print "Index does not exist"

serieExample :: Serie
serieExample = Serie { sid            = 1
                     , stitle         = T.pack "Mob Psycho 100"
                     , soriginalTitle = T.pack "モブサイコ100"
                     , sdirector      = T.pack "Mobu Saiko Hyaku, Yuzuru Tachikawa"
                     , syear          = T.pack "2019, 2019"
                     , epNumber       = T.pack "12, 13"
                     , seNumber       = T.pack "2"
                     , spossession    = T.pack "Oui"
                     , swatched       = T.pack "Oui" }

filmExample :: Film
filmExample = Film { fid            = 1
                   , ftitle         = T.pack "The Truman Show"
                   , foriginalTitle = T.pack "The Truman Show"
                   , fdirector      = T.pack "Peter Weir"
                   , fyear          = T.pack "1998"
                   , fpossession    = "Yes"
                   , fwatched       = "Yes" }
