module Zamonia.Work
    where

import           Database.SQLite.Simple

class Work a where
    title :: a -> String -- ^ Title of the work
    id_ :: a -> Int -- ^ ID of the work
    modWork :: Connection -> Int -> a -> IO () -- ^ Modifying the informations of a work
    addWork :: Connection -> a -> IO () -- ^ Adding a work
    cmpWork :: a -> a -> a -- ^ Comparing two works
    texToLine :: a -> String
    texToEntry :: String -> a -> String
    toFullTex :: IO String -> a -> IO String
    toFullTex = (??) . (texToEntry <$>)

orPrint :: Either String a -> (a -> IO()) -> IO ()
orPrint = flip (either putStrLn)

compareFields :: String -> String -> String
compareFields s1 s2 = if null s1 then s2 else s1

printEmpty :: Show a => [a] -> String
printEmpty []     = "The index you asked for is empty"
printEmpty (x:xs) = show x

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
