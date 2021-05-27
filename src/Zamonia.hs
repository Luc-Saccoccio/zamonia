{-# LANGUAGE OverloadedStrings #-}
module Zamonia where

import           Data.Aeson
import qualified Data.IntMap.Lazy as I
import qualified Data.Text        as T
import           Text.Printf

class Work a where
    title :: a -> T.Text
    addWork :: a -> I.IntMap a -> I.IntMap a
    delWork :: Int -> I.IntMap a -> I.IntMap a
    modWork :: Int -> I.IntMap a -> I.IntMap a
    listWork :: I.IntMap a -> [IO ()]
    listWork = map (putStr . snd) . I.toList . I.mapWithKey toElement
        where
            toElement :: Work w => Int -> w -> String
            toElement key w = printf "%d %s\n" key (title w)
    searchWork :: String -> T.Text -> I.IntMap a -> I.IntMap a

data Serie = Serie
    { stitle         :: T.Text -- Title
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
      = object ["title" .= stitle,
                "originalTitle" .= soriginalTitle,
                "director" .= sdirector,
                "year" .= syear,
                "epNumber" .= epNumber,
                "seNumber" .= seNumber,
                "possession" .= spossession,
                "watched" .= swatched]

instance Show Serie where
    show (Serie t o d y e s p w) = printf "\ESC[1;37mTitle:\ESC[m %s\n\ESC[1;37mOriginal Title:\ESC[m %s\n\ESC[1;37mDirector:\ESC[m %s\n\
    \\ESC[1;37mYear or release:\ESC[m %s\n\ESC[1;37mNumber of episodes:\ESC[m %s\n\ESC[1;37mNumber of seasons:\ESC[m %s\n\ESC[1;37mPossession:\ESC[m %s\n\
    \\ESC[1;37mWatched:\ESC[m %s" t o d y e s p w

instance Work Serie where
    title = stitle
    searchWork field term = I.filter (T.isInfixOf term . fieldContent)
        where fieldContent = case field of
                               "title" -> title
                               "originalTitle" -> soriginalTitle
                               "director" -> sdirector
                               "year" -> syear
                               "epNumber" -> epNumber
                               "seNumber" -> seNumber
                               "possession" -> spossession
                               "watched" -> swatched

data Film = Film
    { ftitle         :: T.Text -- Title
    , foriginalTitle :: T.Text -- Original Title
    , fdirector      :: T.Text -- Director
    , fyear          :: T.Text -- Year of release
    , fpossession    :: T.Text -- Yes/No, Physical/Virtual (for example)
    , fwatched       :: T.Text -- Yes/No
    } deriving Eq

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
      = object ["title" .= ftitle,
                "originalTitle" .= foriginalTitle,
                "director" .= fdirector,
                "year" .= fyear,
                "possession" .= fpossession,
                "watched" .= fwatched]

instance Show Film where
    show (Film t o d y p w) = printf "\ESC[1;37mTitle:\ESC[m %s\n\ESC[1;37mOriginal Title:\ESC[m %s\n\ESC[1;37mDirector:\ESC[m %s\n\
    \\ESC[1;37mYear or release:\ESC[m %s\n\ESC[1;37mPossession:\ESC[m %s\n\
    \\ESC[1;37mWatched:\ESC[m %s" t o d y p w

instance Work Film where
    title = ftitle
    searchWork field term = I.filter (T.isInfixOf term . fieldContent)
        where fieldContent = case term of
                               "title" -> title
                               "originalTitle" -> foriginalTitle
                               "director" -> fdirector
                               "year" -> fyear
                               "possession" -> fpossession
                               "watched" -> fwatched
                               

data Command = Add T.Text T.Text
             | Delete T.Text T.Text
             | Modify T.Text T.Text
             | List T.Text
             | Search T.Text T.Text T.Text
             | Init deriving Show

serieExample :: Serie
serieExample = Serie { stitle         = T.pack "Mob Psycho 100"
                     , soriginalTitle = T.pack "モブサイコ100"
                     , sdirector      = T.pack "Mobu Saiko Hyaku, Yuzuru Tachikawa"
                     , syear          = T.pack "2019, 2019"
                     , epNumber       = T.pack "12, 13"
                     , seNumber       = T.pack "2"
                     , spossession    = T.pack "Oui"
                     , swatched       = T.pack "Oui" }

filmExample :: Film
filmExample = Film { ftitle = T.pack "The Truman Show"
                   , foriginalTitle = T.pack "The Truman Show"
                   , fdirector = T.pack "Peter Weir"
                   , fyear = T.pack "1998"
                   , fpossession = "Yes"
                   , fwatched = "Yes" }
