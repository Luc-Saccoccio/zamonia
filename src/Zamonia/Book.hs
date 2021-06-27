{-# LANGUAGE OverloadedStrings #-}
module Zamonia.Book
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

-- | Structure representing a book
data Book = Book
    { bid            :: Int    -- ^ ID
    , bisbn          :: String -- ^ ISBN
    , btitle         :: String -- ^ Title
    , boriginalTitle :: String -- ^ Original Title
    , bauthor        :: String -- ^ Author
    , bpublisher     :: String -- ^ Publisher
    , byear          :: String -- ^ Year of release
    , bpossession    :: String -- ^ Yes/No, Physical/Virtual (for example)
    , bread          :: String -- ^ Yes/No
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
    toJSON (Book i isbn title originalTitle author publisher year possession watched)
      = object [ "id" .= i
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
        | otherwise = mzero -- | Fail if the number of field if too low

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

-- | Better Show instance => Pretty Print of a Book TODO
instance Show Book where
    show (Book _ is t o a pu y p w) = printf "\ESC[1;37mISBN:\ESC[m %s\n\ESC[1;37mTitle:\ESC[m %s\n\ESC[1;37mOriginal Title:\ESC[m %s\n\ESC[1;37mAuthor:\ESC[m %s\n\
    \\ESC[1;37mPubliser:\ESC[m %s\n\ESC[1;37mYear of release:\ESC[m %s\n\ESC[1;37mPossession:\ESC[m %s\n\
    \\ESC[1;37mWatched:\ESC[m %s" is t o a y p w

instance Work Book where
    title = btitle
    id_ = bid
    addWork conn = execute conn "INSERT OR REPLACE INTO Books VALUES\
                                \ (?,?,?,?,?,?,?)"
    cmpWork (Book i1 is1 t1 o1 a1 pu1 y1 p1 w1) (Book i2 is2 t2 o2 a2 pu2 y2 p2 w2) =
        Book
            { bid = if i2 == -1 then i1 else i2
            , bisbn = compareFields is1 is2
            , btitle = compareFields t1 t2
            , boriginalTitle = compareFields o1 o2
            , bauthor = compareFields a1 a2
            , bpublisher = compareFields pu1 pu2
            , byear = compareFields y1 y2
            , bpossession = compareFields p1 p2
            , bread = compareFields w1 w2
            }
    modWork conn n f = (addWork conn . cmpWork f . head) =<<
        (queryNamed conn "SELECT * FROM Books WHERE IdB = :id" [":id" := n] :: IO [Book])
    replaceList (Book i is t o a pu y p w) = [ Replace "%index%" (show i)
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
delBook conn n = execute conn "DELETE FROM Bookss WHERE IdB = ?" (Only n)

-- | Return a list of all books, sorted the way asked
listBooks :: Sort -> Connection -> IO [(Int, String, String)]
listBooks s conn = query_ conn sql
    where
        sql :: Query
        sql = case s of
                Names -> "SELECT IdB, Done, Title FROM Books ORDER BY Title" -- Sorting by name
                Done -> "SELECT IdB, Done, Title FROM Books ORDER BY Done" -- Sorting by watching state
                Ids -> "SELECT IdB, Done, Title FROM Books" -- Default sort => by index

-- | Print a book (print, ahah :D)
printBook :: Connection -> Int -> IO ()
printBook conn n =
    fetchBooks >>= putStrLn . printEmpty
  where
    fetchBooks :: IO [Book]
    fetchBooks = queryNamed conn sql [":id" := n]
    sql :: Query
    sql = "SELECT * FROM Books WHERE IdB = :id"

-- | Read the specified file, try to decode it. If it fails, print the error.
-- If it didn't fail, add each book to the database.
importBooksJSON :: Connection -> FilePath -> IO ()
importBooksJSON conn = BS.readFile >=> \j -> orPrint (eitherDecode j :: Either String [Book])
                        $ mapM_ (addWork conn)

-- | Read the specified file, try to decode it. If it fails, print the error.
-- If it didn't fail, add each book to the database.
importBooksCSV :: Connection -> FilePath -> IO ()
importBooksCSV conn = BS.readFile >=> \c -> orPrint (C.decode C.HasHeader c :: Either String (Vector Book))
                        $ mapM_ (addWork conn)

-- | Query the book, encode them and write them to the specified file.
exportBooksJSON :: Connection -> FilePath -> IO ()
exportBooksJSON conn file = BS.writeFile file . encodePretty =<< books
    where
        books = query_ conn "SELECT * FROM Books" :: IO [Book]

-- | Query the books, encode them and write them to the specified file.
exportBooksCSV :: Connection -> FilePath -> IO ()
exportBooksCSV conn file = BS.writeFile file . C.encode =<< books
    where
        books = query_ conn "SELECT * FROM Books" :: IO [Book]

-- | Delete all entries in Books table
purgeBooks :: Connection -> IO ()
purgeBooks conn = execute_ conn "DELETE FROM Books"

-- | Convert each entry to a formatted string, and concatenate
booksToFullFormatted :: FilePath -> Connection -> IO String
booksToFullFormatted file conn = fmap concat . mapM toFormatted =<< books
    where
        toFormatted :: Book -> IO String
        toFormatted = toFullFormatted template -- | Prepare with the template
        template :: IO String
        template = readFile file
        books :: IO [Book]
        books = query_ conn "SELECT * FROM Books"
