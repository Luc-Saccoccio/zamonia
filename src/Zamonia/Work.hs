module Zamonia.Work
    where

import           Database.SQLite.Simple (Connection)
import           Text.Replace           (Replace)

class Work a where
    title :: a -> String -- ^ Title of the work
    id_ :: a -> Int -- ^ ID of the work
    modWork :: Connection -> Int -> a -> IO () -- ^ Modifying the informations of a work
    addWork :: Connection -> a -> IO () -- ^ Adding a work
    cmpWork :: a -> a -> a -- ^ Comparing two works
    replaceList :: a -> [Replace] -- ^ List containing replacing information for a specific work
    entryToFormatted :: String -> a -> String -- ^ Convert an entry to a formatted string
    toFullFormatted :: IO String -> a -> IO String
    toFullFormatted = (??) . (entryToFormatted <$>)

-- | If an error occurs, print it, else process the result
orPrint :: Either String a -> (a -> IO()) -> IO ()
orPrint = flip (either putStrLn)

-- | Comapre two fields in the structures
compareFields :: String -> String -> String
compareFields s1 s2 = if null s1 then s2 else s1

-- | If the list if empty, explain the cause, else return the (showed) head
printEmpty :: Show a => [a] -> String
printEmpty []     = "The index you asked for is empty"
printEmpty (x:xs) = show x

-- | Don't want to import (or even have as a dependency) Control.Lens, so redefining (??)
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab

-- | Types of sort
data Sort = Ids
          | Names
          | Watched

-- | Select sort
(<~>) :: Sort -> Sort -> Sort
(<~>) _ Names   = Names
(<~>) _ Watched = Watched
(<~>) a Ids     = a
