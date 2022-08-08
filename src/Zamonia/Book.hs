{-# LANGUAGE OverloadedStrings #-}
module Zamonia.Book
    where

import           Control.Monad            (mzero, (>=>))
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as BS
import qualified Data.Csv                 as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           Data.Vector              (Vector)
import           Database.SQLite.Simple
import           Text.Printf
import           Text.Replace
import           Zamonia.Work

-- | Structure representing a book
data Book = Book
    { _bid            :: Int    -- ^ ID
    , _bisbn          :: T.Text -- ^ ISBN
    , _btitle         :: T.Text -- ^ Title
    , _boriginalTitle :: T.Text -- ^ Original Title
    , _bauthor        :: T.Text -- ^ Author
    , _bpublisher     :: T.Text -- ^ Publisher
    , _byear          :: T.Text -- ^ Year of release
    , _bpossession    :: T.Text -- ^ Yes/No, Physical/Virtual (for example)
    , _bread          :: T.Text -- ^ Yes/No
    } deriving Eq

-- | Commands related to books
data BooksCommand =
            BAdd Book
             | BDelete Int
             | BPrint Int
             | BModify Int Book
             | BSearch String String
             | BImportCSV FilePath
             | BImportJSON FilePath
             | BExportJSON FilePath
             | BExportCSV FilePath
             | BExportFormatted FilePath FilePath
             | BList Sort
             | BPurge

-- | Instance to allow parsing JSON for Book
instance FromJSON Book where
    parseJSON (Object v) = Book <$>
        v .: "id" <*>
        v .: "isbn" <*>
        v .: "title" <*>
        v .: "originalTitle" <*>
        v .: "author" <*>
        v .: "publisher" <*>
        v .: "year" <*>
        v .: "possession" <*>
        v .: "read"
    parseJSON _ = mzero

-- | Instance to allow transforming a Book to a JSON entry
instance ToJSON Book where
    toJSON (Book bid isbn title originalTitle author publisher year possession watched)
      = object [ "id" .= bid
               , "isbn" .= isbn
               , "title" .= title
               , "originalTitle" .= originalTitle
               , "author" .= author
               , "publisher" .= publisher
               , "year" .= year
               , "possession" .= possession
               , "watched" .= watched]

-- | Instance to allow parsing CSV for Book
instance C.FromRecord Book where
    parseRecord v
        | length v >= 9 = Book <$> v C..! 0 <*> v C..! 1 <*> v C..! 2 <*> v C..! 3
                                    <*> v C..! 4 <*> v C..! 5 <*> v C..! 6
                                    <*> v C..! 7 <*> v C..! 8
        | otherwise = mzero -- Fail if the number of field if too low

-- | Instance to allow transforming a Book to a CSV line
instance C.ToRecord Book where
    toRecord (Book i is t o a pu y p w) = C.record [C.toField i, C.toField is, C.toField t,
        C.toField o, C.toField a, C.toField pu, C.toField y, C.toField p, C.toField w]

-- | Instance to allow reading a row as a Book
instance FromRow Book where
  fromRow = Book <$> field <*> field <*> field <*> field
             <*> field <*> field <*> field <*> field <*> field

-- | Instance to transform a book into a row
instance ToRow Book where
    toRow (Book i is t o a pu y p w) = toRow (i, is, t, o, a, pu, y, p, w)

-- | Better Show instance => Pretty Print of a Book
instance Show Book where
    show (Book _ is t o a pu y p r) = printf "ISBN: %s\nTitle: %s\nOriginal Title: %s\nAuthor: %s\n\
    \Publiser: %s\nYear of release: %s\nPossession: %s\n\
    \Read: %s" is t o a pu y p r

instance Work Book where
    new = Book { _bid = 0
               , _bisbn = T.empty
               , _btitle = T.empty
               , _boriginalTitle = T.empty
               , _bauthor = T.empty
               , _bpublisher = T.empty
               , _byear = T.empty
               , _bpossession = T.empty
               , _bread = T.empty
               }

    title = _btitle
    status = _bread
    id_ = _bid
    addWork conn = execute conn "INSERT OR REPLACE INTO Books VALUES\
                                \ (?,?,?,?,?,?,?,?,?)"
    cmpWork (Book i1 is1 t1 o1 a1 pu1 y1 p1 w1) (Book i2 is2 t2 o2 a2 pu2 y2 p2 w2) =
        Book
            { _bid = if i2 == -1 then i1 else i2
            , _bisbn = compareFields is1 is2
            , _btitle = compareFields t1 t2
            , _boriginalTitle = compareFields o1 o2
            , _bauthor = compareFields a1 a2
            , _bpublisher = compareFields pu1 pu2
            , _byear = compareFields y1 y2
            , _bpossession = compareFields p1 p2
            , _bread = compareFields w1 w2
            }
    queryAll conn = query_ conn "SELECT * FROM Books"
    fetchWork conn n = queryNamed conn sql [":id" := n]
      where
        sql :: Query
        sql = "SELECT * FROM Books WHERE IdB = :id"
    modWork conn n f = (addWork conn . cmpWork f . head) =<<
        (queryNamed conn "SELECT * FROM Books WHERE ISBN = :id" [":id" := n] :: IO [Book])
    replaceList (Book i is t o a pu y p w) = [ Replace "%index%" (T.pack $ show i)
                                           , Replace "%isbn%" is
                                           , Replace "%title%" t
                                           , Replace "%originalTitle%" o
                                           , Replace "%author%" a
                                           , Replace "%publisher%" pu
                                           , Replace "%year%" y
                                           , Replace "%possession%" p
                                           , Replace "%read%" w]

-- | Delete the book matching the index
delBook :: Connection -> Int -> IO ()
delBook conn n = execute conn "DELETE FROM Books WHERE IdB = ?" (Only n)

-- | Return a list of all books, sorted the way asked
listBooks :: Sort -> Connection -> IO [(Int, T.Text, T.Text)]
listBooks s conn = query_ conn sql
    where
        sql :: Query
        sql = case s of
                Names -> "SELECT IdB, Done, Title FROM Books ORDER BY Title" -- Sorting by name
                Done -> "SELECT IdB, Done, Title FROM Books ORDER BY Done" -- Sorting by watching state
                Ids -> "SELECT IdB, Done, Title FROM Books" -- Default sort => by index

-- | Delete all entries in Books table
purgeBooks :: Connection -> IO ()
purgeBooks conn = execute_ conn "DELETE FROM Books"
