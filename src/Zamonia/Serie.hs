{-# LANGUAGE OverloadedStrings #-}
module Zamonia.Serie where

import           Control.Monad            (mzero, (>=>))
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as BS
import qualified Data.Csv                 as C
import           Data.Vector              (Vector)
import           Database.SQLite.Simple
import           Text.Printf
import           Zamonia.Work


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
             | SExportFullTex FilePath FilePath
             | SList Sort
             | SPurge

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
            { sid = if i1 == -1 then i2 else i1
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
    texToLine (Serie i t o d y e s p w) = printf "%d & %s & %s & %s & %s & %s & %s & %s & %s"
                                            i t o d y e s p w
    texToEntry c (Serie i t o d y e s p w) = printf c i t o d y e s p w

delSerie :: Connection -> Int -> IO ()
delSerie conn n = execute conn "DELETE FROM Series WHERE IdS = ?" (Only n)

listSeries :: Sort -> Connection -> IO [(Int, String, String)]
listSeries s conn = query_ conn sql
    where
        sql :: Query
        sql = case s of
                Names -> "SELECT IdS, Watched, Title FROM Series ORDER BY Title"
                Watched -> "SELECT IdS, Watched, Title FROM Series ORDER BY Watched"
                Ids -> "SELECT IdS, Watched, Title FROM Series"

printSerie :: Connection -> Int -> IO ()
printSerie conn n =
    fetchSeries >>= putStrLn . printEmpty
        where
    fetchSeries :: IO [Serie]
    fetchSeries = queryNamed conn sql [":id" := n]
    sql = "SELECT * FROM Series WHERE IdS = :id"

-- | Read the specified file, try to decode it. If it fails, print the error.
-- If it didn't fail, add each work to the database.
importSeriesJSON :: Connection -> FilePath -> IO ()
importSeriesJSON conn = BS.readFile >=> \j -> orPrint (eitherDecode j :: Either String [Serie])
                        $ mapM_ (addWork conn)

-- | Read the specified file, try to decode it. If it fails, print the error.
-- If it didn't fail, add each work to the database.
importSeriesCSV :: Connection -> FilePath -> IO ()
importSeriesCSV conn = BS.readFile >=> \c -> orPrint (C.decode C.HasHeader c :: Either String (Vector Serie))
                        $ mapM_ (addWork conn)

-- | Query the series, encode them and write them to the specified file.
exportSeriesJSON :: Connection -> FilePath -> IO ()
exportSeriesJSON conn file = BS.writeFile file . encodePretty =<< series
    where
        series = query_ conn "SELECT * FROM Series" :: IO [Serie]

-- | Query the series, encode them and write them to the specified file.
exportSeriesCSV :: Connection -> FilePath -> IO ()
exportSeriesCSV conn file = BS.writeFile file . C.encode =<< series
    where
        series = query_ conn "SELECT * FROM Series" :: IO [Serie]

purgeSeries :: Connection -> IO ()
purgeSeries conn = execute_ conn "DELETE FROM Series"

serieToFullTex :: FilePath -> Connection -> IO String
serieToFullTex file conn = fmap concat . mapM toTex =<< series
    where
        toTex :: Serie -> IO String
        toTex = toFullTex template
        template :: IO String
        template = readFile file
        series :: IO [Serie]
        series = query_ conn "SELECT * FROM Series"
