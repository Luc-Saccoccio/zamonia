{-# LANGUAGE OverloadedStrings #-}
module Zamonie where

import           Data.Aeson
import qualified Data.Text     as T

data Serie = Serie
    { stitle         :: T.Text -- Title
    , soriginalTitle :: T.Text -- Original Title
    , sdirector      :: T.Text -- Director
    , syear          :: T.Text -- Year of release
    , epNumber       :: T.Text -- Number of episodes
    , seNumber       :: T.Text -- Number of seasons
    , spossession    :: T.Text -- Yes/No, Physical/Virtual (for example)
    , swatched       :: Bool -- Yes/No
    } deriving (Show, Eq)

instance FromJSON Serie where
    parseJSON (Object v) = Serie <$>
                    v .: "title" <*>
                    v .: "originalTitle" <*>
                    v .: "director" <*>
                    v .: "year" <*>
                    v .: "epNumber" <*>
                    v .: "seNumber" <*>
                    v .: "possession" <*>
                    v .: "watched"
    -- parseJSON _ = mzero -- TODO : Fix this

instance ToJSON Serie where
    toJSON (Serie stitle soriginalTitle sdirector syear epNumber seNumber spossession swatched)
      = object ["title" .= stitle, "originalTitle" .= soriginalTitle, "director" .= sdirector,
      "year" .= syear, "epNumber" .= epNumber, "seNumber" .= seNumber, "possession" .= spossession,
      "watched" .= swatched]

data Film = Film
    { ftitle         :: T.Text -- Title
    , foriginalTitle :: T.Text -- Original Title
    , fdirector      :: T.Text -- Director
    , fyear          :: T.Text -- Year of release
    , fpossession    :: T.Text -- Yes/No, Physical/Virtual (for example)
    , fwatched       :: Bool -- Yes/No
    } deriving (Show, Eq)

instance FromJSON Film where
    parseJSON (Object v) = Film <$>
        v .: "title" <*>
        v .: "originalTitle" <*>
        v .: "director" <*>
        v .: "year" <*>
        v .: "possession" <*>
        v .: "watched"
    -- parseJSON _ = mzero -- TODO : Fix this

instance ToJSON Film where
    toJSON (Film ftitle foriginalTitle fdirector fyear fpossession fwatched)
      = object ["title" .= ftitle, "originalTitle" .= foriginalTitle, "director" .= fdirector,
      "year" .= fyear, "possession" .= fpossession, "watched" .= fwatched]

data Command = Add T.Text T.Text
             | Delete T.Text T.Text
             | Modify T.Text T.Text
             | List T.Text
             | Search T.Text T.Text T.Text
             | Init

serieExample :: Serie
serieExample = Serie { stitle         = T.pack "Mob Psycho 100"
                     , soriginalTitle = T.pack "モブサイコ100"
                     , sdirector      = T.pack "Mobu Saiko Hyaku, Yuzuru Tachikawa"
                     , syear          = T.pack "2019, 2019"
                     , epNumber       = T.pack "12, 13"
                     , seNumber       = T.pack "2"
                     , spossession    = T.pack "Oui"
                     , swatched       = True }

filmExample :: Film
filmExample = Film { ftitle = T.pack "The Truman Show"
                   , foriginalTitle = T.pack "The Truman Show"
                   , fdirector = T.pack "Peter Weir"
                   , fyear = T.pack "1998"
                   , fpossession = "Yes"
                   , fwatched = True }
