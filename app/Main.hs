{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BS
import           Data.IntMap.Lazy     (IntMap, (!?))
import           Data.Semigroup       ((<>))
import           Options.Applicative
import           System.Directory
import           Text.Printf
import           Zamonia

data Usage = Init
           | Films FilmsCommand
           | Series SeriesCommand


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

subCommandsFilms :: Parser FilmsCommand
subCommandsFilms = subparser $
              cmd add  "add" "Add work to list"
           <> cmd del  "delete" "Delete first matching work from list"
           <> cmd show "show" "Show informations about specified index"
           <> cmd mod  "modify" "Modify first matching work from list"
           <> command  "list" (info (pure FList) (progDesc "List entries from list"))
           <> cmd search "search" "Search keyword in list"
               where
                   tTitle = strOption ( long "title"
                                        <> short 't'
                                        <> metavar "TITLE"
                                        <> value ""
                                        <> help "Title" )
                   original = strOption ( long "original"
                                        <> short 'o'
                                        <> metavar "TITLE"
                                        <> value ""
                                        <> help "Original Title")
                   director = strOption ( long "director"
                                        <> short 'd'
                                        <> metavar "DIRECTOR"
                                        <> value ""
                                        <> help "Director")
                   year = strOption ( long "year"
                                        <> short 'y'
                                        <> metavar "YEAR"
                                        <> value ""
                                        <> help "Year of release")
                   possession = strOption ( long "possession"
                                        <> short 'p'
                                        <> metavar "POSSESSION"
                                        <> value ""
                                        <> help "Weither your possess it or not, and how")
                   watched = strOption ( long "watched"
                                        <> short 'w'
                                        <> metavar "WATCHED"
                                        <> value ""
                                        <> help "Weither you watched it or not")
                   add       = FAdd    <$> (Film <$> strArg "TITLE" "Title of the work you want to add" <*> original <*> director <*> year <*> possession <*> watched)
                   del       = FDelete <$> argument auto (metavar "INDEX" <> help "Index to delete, must be an integer")
                   show      = FPrint <$> argument auto (metavar "INDEX" <> help "Index to show, must be an integer")
                   mod       = FModify <$> argument auto (metavar "INDEX" <> help "Index to modify, must be an integer")
                                            <*> (Film <$> tTitle <*> original <*> director <*> year <*> possession <*> watched)
                   search    = FSearch <$> strArg "FIELD" "In what field (e.g. title/year) the search will be done" <*> strArg "SEARCH" "Thing to search for"
                   cmd p n d = command n (info (helper <*> p) (progDesc d))
                   strArg n h  = strArgument (metavar n <> help h)

subCommandsSeries :: Parser SeriesCommand
subCommandsSeries = subparser $
              cmd add  "add" "Add work to list"
           <> cmd del  "delete" "Delete first matching work from list"
           <> cmd show "show" "Show informations about specified index"
           <> cmd mod  "modify" "Modify first matching work from list"
           <> command  "list" (info (pure SList) (progDesc "List entries from list"))
           <> cmd search "search" "Search keyword in list"
               where
                   tTitle = strOption ( long "title"
                                        <> short 't'
                                        <> metavar "TITLE"
                                        <> value ""
                                        <> help "Title" )
                   original = strOption ( long "original"
                                        <> short 'o'
                                        <> metavar "TITLE"
                                        <> value ""
                                        <> help "Original Title")
                   director = strOption ( long "director"
                                        <> short 'd'
                                        <> metavar "DIRECTOR"
                                        <> value ""
                                        <> help "Director")
                   year = strOption ( long "year"
                                        <> short 'y'
                                        <> metavar "YEAR"
                                        <> value ""
                                        <> help "Year of release")
                   episodesNumber = strOption ( long "episodes-number"
                                        <> short 'n'
                                        <> metavar "NUMBER"
                                        <> value ""
                                        <> help "Number of episodes")
                   seasonsNumber = strOption ( long "seasons-number"
                                        <> short 's'
                                        <> metavar "NUMBER"
                                        <> value ""
                                        <> help "Number of seasons")
                   possession = strOption ( long "possession"
                                        <> short 'p'
                                        <> metavar "POSSESSION"
                                        <> value ""
                                        <> help "Weither your possess it or not, and how")
                   watched = strOption ( long "watched"
                                        <> short 'w'
                                        <> metavar "WATCHED"
                                        <> value ""
                                        <> help "Weither you watched it or not")
                   add       = SAdd    <$> (Serie <$> strArg "TITLE" "Title of the work you want to add" <*> original <*> director <*> year
                                                        <*> episodesNumber <*> seasonsNumber <*> possession <*> watched)
                   del       = SDelete <$> argument auto (metavar "INDEX" <> help "Index to delete, must be an integer")
                   show      = SPrint <$> argument auto (metavar "INDEX" <> help "Index to show, must be an integer")
                   mod       = SModify <$> argument auto (metavar "INDEX" <> help "Index to modify, must be an integer")
                                                        <*> (Serie <$> tTitle <*> original <*> director <*> year
                                                        <*> episodesNumber <*> seasonsNumber <*> possession <*> watched)
                   search    = SSearch <$> strArg "FIELD" "In what field (e.g. title/year) the search will be done" <*> strArg "SEARCH" "Thing to search for"
                   cmd p n d = command n (info (helper <*> p) (progDesc d))
                   strArg n h  = strArgument (metavar n <> help h)

usage :: Parser Usage
usage = subparser $
       command "film"  (Films <$> subCommandsFilms `withInfo` "Work on Film list")
    <> command "serie" (Series <$> subCommandsSeries `withInfo` "Work on Serie list")
    <> command "init"  (pure Init `withInfo` "Initiate a Zamonia database")

readJson :: String -> IO (Either String BS.ByteString)
readJson file = do
    exist <- doesFileExist file
    if exist then
             do
                 content <- BS.readFile file
                 return $ Right content
    else
        return . Left $ printf "List %s do not exist. Please create it" file

runFilms :: FilmsCommand -> IO ()
runFilms c = readJson "films.json" >>= \films ->
    orPrint films $ \rawList ->
        let json = eitherDecode rawList :: Either String (IntMap Film) in
        orPrint json $ \list ->
          case c of
           FAdd f      -> BS.writeFile "films.json" (encodePretty $ addWork f list)
           FDelete i   -> BS.writeFile "films.json" (encodePretty $ delWork i list)
           FPrint i    -> orIndexError (list !? i) print
           FModify n f   -> BS.writeFile "films.json" (encodePretty $ modWork n f list)
           FSearch f t -> sequence_ . listWork . searchWork f t $ list
           FList       -> sequence_ $ listWork list

runSeries :: SeriesCommand -> IO ()
runSeries c = readJson "series.json" >>= \films ->
    orPrint films $ \rawList ->
        let json = eitherDecode rawList :: Either String (IntMap Serie) in
        orPrint json $ \list ->
          case c of
           SAdd s      -> BS.writeFile "series.json" (encodePretty $ addWork s list)
           SDelete i   -> BS.writeFile "series.json" (encodePretty $ delWork i list)
           SPrint i    -> orIndexError (list !? i) print
           SModify n s   -> BS.writeFile "series.json" (encodePretty $ modWork n s list)
           SSearch f t -> sequence_ . listWork . searchWork f t $ list
           SList       -> sequence_ $ listWork list

main :: IO ()
main = do
    execution <- execParser (info (helper <*> usage)
            ( fullDesc
            <> progDesc "CLI personal library database"
            <> header "zamonia - a CLI personal library database written in haskell" ))
    case execution of
      Init -> do
          createDirectory "lists"
          writeFile "lists/series.json" ""
          writeFile "lists/films.json" ""
      Films c -> runFilms c
      Series c -> return ()
