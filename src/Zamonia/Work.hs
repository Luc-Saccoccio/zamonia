{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Zamonia.Work
    where

import           Control.Monad            ((>=>))
import           Data.Aeson               (FromJSON, ToJSON, eitherDecode')
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as BS (readFile, writeFile)
import           Data.Csv                 (FromRecord, HasHeader (..), ToRecord,
                                           decode, encode)
import           Data.String              (IsString (fromString))
import qualified Data.Text                as T (Text, null)
import qualified Data.Text.Lazy           as L (Text, concat, pack)
import           Data.Vector              (Vector)
import           Database.SQLite.Simple   (Connection, FromRow, Query, execute, execute_,
                                           query_)
import           Text.Printf
import           Text.Replace             (Replace, replaceWithList)

data Id =  IdF Int | IdS Int | IdB Int

type Cons work = (Work work, FromJSON work, ToJSON work, FromRecord work, ToRecord work, Show work, FromRow work)

class Work a where
    new :: a
    status :: a -> T.Text
    title :: a -> T.Text -- ^ Title of the work
    id_ :: a -> Int -- ^ ID of the work
    listRepresentation :: a -> (Int, T.Text, T.Text)
    listRepresentation w = (id_ w, status w, title w)
    addWork :: Connection -> a -> IO () -- ^ Adding a work
    cmpWork :: a -> a -> a -- ^ Comparing two works
    queryAll :: Connection -> IO [a]
    replaceList :: a -> [Replace] -- ^ List containing replacing information for a specific work
    entryToFormatted :: L.Text -> a -> L.Text -- ^ Convert an entry to a formatted string
    entryToFormatted c w = replaceWithList (replaceList w) c
    toFullFormatted :: IO L.Text -> a -> IO L.Text
    toFullFormatted = (??) . (entryToFormatted <$>)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

-- | Read the specified file, try to decode it. If it fails, print the error.
-- If it didn't fail, add each entry to the database.
importJSON :: (Work work, FromJSON work) => proxy work -> Connection -> FilePath -> IO ()
importJSON (_ :: proxy work) conn = BS.readFile >=> \j -> orPrint (eitherDecode' j :: Either String [work])
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
allToFullFormatted (_ :: proxy work) file conn = fmap L.concat . mapM toFormatted =<< queryAll conn
    where
        -- toFormatted :: Work a => a -> IO L.Text
        toFormatted = toFullFormatted @work template
        template :: IO L.Text
        template = L.pack <$> readFile file

delWork :: Connection -> Id -> IO ()
delWork conn n = execute_ conn . fromString $ uncurry3 (printf sql) infos
  where
    infos :: (T.Text, T.Text, Int)
    infos =
      case n of
        (IdF x) -> ("Films", "IdF", x)
        (IdS x) -> ("Series", "IdS", x)
        (IdB x) -> ("Books", "IdB", x)
    sql :: String
    sql = "DELETE FROM %s WHERE %s = %d"

fetchWork :: (FromRow w, Work w) => Connection -> Id -> IO [w] -- TODO: Type returned isn't fixed
fetchWork conn n = query_ conn . fromString $ uncurry3 (printf sql) infos
  where
    infos :: (T.Text, T.Text, Int)
    infos =
      case n of
        (IdF x) -> ("Films", "IdF", x)
        (IdS x) -> ("Series", "IdS", x)
        (IdB x) -> ("Books", "IdB", x)
    sql :: String
    sql = "SELECT * FROM %s WHERE %s = %d"

modWork :: (FromRow w, Work w) => Connection -> Id -> w -> IO () -- ^ Modifying the informations of a work
modWork conn n s = addWork conn . cmpWork s . head =<< fetchWork conn n

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

-- | TODO
printWork :: Show a => [a] -> IO ()
printWork = putStrLn . printEmpty

(??) :: Functor f => f (a -> b) -> a -> f b
f ?? a = fmap ($ a) f

-- | Types of sort
data Sort = Ids
          | Names
          | Done

-- | Select sort
(<~>) :: Sort -> Sort -> Sort
(<~>) _ Names = Names
(<~>) _ Done  = Done
(<~>) a Ids   = a
