{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zamonia.Work
    where

import           Control.Monad            ((>=>))
import           Data.Aeson               (FromJSON, ToJSON, eitherDecode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as BS (readFile, writeFile)
import           Data.Csv                 (FromRecord, HasHeader (..), ToRecord,
                                           decode, encode)
import           Data.Proxy
import qualified Data.Text                as T (Text, null)
import qualified Data.Text.Lazy           as L (Text, concat, pack)
import           Data.Vector              (Vector)
import           Database.SQLite.Simple   (Connection)
import           Text.Replace             (Replace, replaceWithList)


class Work a where
    new :: a
    title :: a -> T.Text -- ^ Title of the work
    id_ :: a -> String -- ^ ID of the work
    modWork :: Connection -> Int -> a -> IO () -- ^ Modifying the informations of a work
    addWork :: Connection -> a -> IO () -- ^ Adding a work
    cmpWork :: a -> a -> a -- ^ Comparing two works
    queryAll :: Connection -> IO [a]
    replaceList :: a -> [Replace] -- ^ List containing replacing information for a specific work
    entryToFormatted :: L.Text -> a -> L.Text -- ^ Convert an entry to a formatted string
    entryToFormatted c w = replaceWithList (replaceList w) c
    toFullFormatted :: IO L.Text -> a -> IO L.Text
    toFullFormatted = (??) . (entryToFormatted <$>)

-- | Read the specified file, try to decode it. If it fails, print the error.
-- If it didn't fail, add each entry to the database.
importJSON :: (Work work, FromJSON work) => proxy work -> Connection -> FilePath -> IO ()
importJSON (_ :: proxy work) conn = BS.readFile >=> \j -> orPrint (eitherDecode j :: Either String [work])
                        $ mapM_ (addWork conn)

-- | Read the specified file, try to decode it. If it fails, print the error.
-- If it didn't fail, add each entry to the database.
importCSV :: (Work work, FromRecord work) => proxy work -> Connection -> FilePath -> IO ()
importCSV (_ :: proxy work) conn = BS.readFile >=> \c -> orPrint (decode HasHeader c :: Either String (Vector work)) $ mapM_ (addWork conn)

exportJSON :: (Work work, ToJSON work) => proxy work -> Connection -> FilePath -> IO ()
exportJSON (_ :: proxy work) conn file = BS.writeFile file . encodePretty =<< (queryAll conn :: IO [work])

exportCSV :: (Work work, ToRecord work) => proxy work -> Connection -> FilePath -> IO ()
exportCSV (_ :: proxy work) conn file = BS.writeFile file . encode =<< (queryAll conn :: IO [work])

-- | Convert each entry to a formatted string, and concatenate
allToFullFormatted :: Work work => proxy work -> FilePath -> Connection -> IO L.Text
allToFullFormatted (_ :: proxy work) file conn = fmap L.concat . mapM (toFormatted :: work -> IO L.Text) =<< queryAll conn
    where
        -- toFormatted :: Work a => a -> IO L.Text
        toFormatted = toFullFormatted template
        template :: IO L.Text
        template = L.pack <$> readFile file

-- | If an error occurs, print it, else process the result
orPrint :: Either String a -> (a -> IO()) -> IO ()
orPrint = flip (either putStrLn)

-- | Comapre two fields in the structures
compareFields :: T.Text -> T.Text -> T.Text
compareFields s1 s2 = if T.null s1 then s2 else s1

-- | If the list if empty, explain the cause, else return the (showed) head
printEmpty :: Show a => [a] -> String
printEmpty []    = "The index you asked for is empty"
printEmpty (x:_) = show x

-- | Don't want to import (or even have as a dependency) Control.Lens, so redefining (??)
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab

-- | Types of sort
data Sort = Ids
          | Names
          | Done

-- | Select sort
(<~>) :: Sort -> Sort -> Sort
(<~>) _ Names = Names
(<~>) _ Done  = Done
(<~>) a Ids   = a
