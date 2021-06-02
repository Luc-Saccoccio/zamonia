{-# LANGUAGE OverloadedStrings #-}
module Zamonia where

import           Data.Aeson
import qualified Data.IntMap.Lazy as I
import           Data.Sort
import qualified Data.Text        as T
import           Text.Printf

class Work a where
    fieldContent :: String -> a -> T.Text
    title :: a -> T.Text
    modWork :: Int -> a -> I.IntMap a -> I.IntMap a
    addWork :: a -> I.IntMap a -> I.IntMap a
    addWork work intmap = case I.lookupMax intmap of
                            Just (k,_) -> I.insert (k+1) work intmap
                            Nothing    -> I.insert 0 work intmap
    delWork :: Int -> I.IntMap a -> I.IntMap a
    delWork = I.delete
    -- modWork :: Int -> I.IntMap a -> I.IntMap a
    listWork :: I.IntMap a -> [IO ()]
    listWork = map (putStr . snd) . I.toList . I.mapWithKey toElement
        where
            toElement :: Work w => Int -> w -> String
            toElement key w = printf "%d %s\n" key (title w)
    searchWork :: String -> T.Text -> I.IntMap a -> I.IntMap a
    searchWork field term = I.filter (T.isInfixOf term . fieldContent field)
    sortWork :: String -> I.IntMap a -> I.IntMap a
    sortWork field = I.fromList . reNumber 0 . sortOn extract . I.toList
        where
            reNumber :: Work a => I.Key -> [(I.Key, a)] -> [(I.Key, a)]
            reNumber _ [] = []
            reNumber k ((_,x):xs) = reNumber (k+1) ((k,x):xs)
            extract :: Work a => (I.Key, a) -> T.Text
            extract = fieldContent field . snd

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
    fieldContent field = case field of
                     "title"         -> title
                     "originalTitle" -> soriginalTitle
                     "director"      -> sdirector
                     "year"          -> syear
                     "epNumber"      -> epNumber
                     "seNumber"      -> seNumber
                     "possession"    -> spossession
                     "watched"       -> swatched

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
    fieldContent field = case field of
                     "title"         -> title
                     "originalTitle" -> foriginalTitle
                     "director"      -> fdirector
                     "year"          -> fyear
                     "possession"    -> fpossession
                     "watched"       -> fwatched

data FilmsCommand = FAdd Film
             | FDelete Int
             | FPrint Int
             | FModify Int Film
             | FSearch String T.Text -- Field then term to search
             | FList deriving Show

data SeriesCommand = SAdd Serie
             | SDelete Int
             | SPrint Int
             | SModify Int Serie
             | SSearch String T.Text
             | SList deriving Show

orPrint :: Either String a -> (a -> IO()) -> IO ()
orPrint = flip (either putStrLn)

orIndexError :: Maybe a -> (a -> IO ()) -> IO ()
orIndexError s f = case s of
                     Just x  -> f x
                     Nothing -> print "Index does not exist"

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
