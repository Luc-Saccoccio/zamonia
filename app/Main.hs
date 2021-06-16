{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad          ((>=>))
import           Data.Semigroup         ((<>))
import           Database.SQLite.Simple
import           Options.Applicative
import           System.Directory
import           Text.Printf
import           Zamonia

data Usage = Init
           | Films FilmsCommand
           | Series SeriesCommand

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

index :: Parser Int
index = argument auto (metavar "INDEX" <> help "Index to add")

tTitle :: Parser String
tTitle = strOption ( long "title"
                <> short 't'
                <> metavar "TITLE"
                <> value ""
                <> help "Title" )

original :: Parser String
original = strOption ( long "original"
                <> short 'o'
                <> metavar "TITLE"
                <> value ""
                <> help "Original Title")

director :: Parser String
director = strOption ( long "director"
                <> short 'd'
                <> metavar "DIRECTOR"
                <> value ""
                <> help "Director")

year :: Parser String
year = strOption ( long "year"
                <> short 'y'
                <> metavar "YEAR"
                <> value ""
                <> help "Year of release")

possession :: Parser String
possession = strOption ( long "possession"
                <> short 'p'
                <> metavar "POSSESSION"
                <> value ""
                <> help "Weither your possess it or not, and how")

watched :: Parser String
watched = strOption ( long "watched"
                <> short 'w'
                <> metavar "WATCHED"
                <> value ""
                <> help "Weither you watched it or not")

episodesNumber :: Parser String
episodesNumber = strOption ( long "episodes-number"
                <> short 'n'
                <> metavar "NUMBER"
                <> value ""
                <> help "Number of episodes")

seasonsNumber :: Parser String
seasonsNumber = strOption ( long "seasons-number"
                <> short 's'
                <> metavar "NUMBER"
                <> value ""
                <> help "Number of seasons")

importCSV :: Parser String
importCSV = strArgument (metavar "FILE" <> help "File to Import, must be a CSV")

exportJSON :: Parser String
exportJSON = strArgument (metavar "FILE" <> help "Destination of the export")

subCommandsFilms :: Parser FilmsCommand
subCommandsFilms = subparser $
              command "add"    (info (helper <*> add) (progDesc "Add work to list"))
           <> command "delete" (info (helper <*> del) (progDesc "Delete first matching work from list"))
           <> command "show"   (info (helper <*> shw) (progDesc "Show informations about specified index"))
           <> command "modify" (info (helper <*> mod) (progDesc "Modify first matching work from list"  ))
           <> command "import" (info (helper <*> imp) (progDesc "Import a list"))
           <> command "export" (info (helper <*> exp) (progDesc "Export a list"))
           <> command "list"   (info (pure FList) (progDesc "List entries from list"))
           <> command "search" (info (helper <*> search) (progDesc "Search keyword in list"))
               where
                   add       = FAdd    <$> (Film <$> index <*> strArgument (metavar "TITLE" <> help "Title of the work you want to add")
                                                    <*> original <*> director <*> year <*> possession <*> watched)
                   del       = FDelete <$> argument auto (metavar "INDEX" <> help "Index to delete, must be an integer")
                   shw      = FPrint  <$> argument auto (metavar "INDEX" <> help "Index to show, must be an integer")
                   mod       = FModify <$> index
                                            <*> (Film <$> argument auto (metavar "INDEX")
                                            <*> tTitle <*> original <*> director <*> year <*> possession <*> watched)
                   imp       = FImport <$> importCSV
                   exp       = FExport <$> exportJSON
                   search    = FSearch <$> strArgument (metavar "FIELD" <> help "In what field (e.g. title/year) the search will be done") <*> strArgument (metavar "SEARCH" <> help "Thing to search for")

subCommandsSeries :: Parser SeriesCommand
subCommandsSeries = subparser $
              command "add"    (info (helper <*> add) (progDesc "Add work to list"))
           <> command "delete" (info (helper <*> del) (progDesc "Delete first matching work from list"))
           <> command "show"   (info (helper <*> shw) (progDesc "Show informations about specified index"))
           <> command "modify" (info (helper <*> mod) (progDesc "Modify first matching work from list"))
           <> command "import" (info (helper <*> imp) (progDesc "Import a list"))
           <> command "export" (info (helper <*> exp) (progDesc "Export a list"))
           <> command "list"   (info (pure SList) (progDesc "List entries from list"))
           <> command "search" (info (helper <*> search) (progDesc "Search keyword in list"))
               where
                   add       = SAdd    <$> (Serie <$> index <*> strArgument (metavar "TITLE" <> help "Title of the work you want to add")
                                                    <*> original <*> director <*> year <*> episodesNumber <*> seasonsNumber <*> possession <*> watched)
                   del       = SDelete <$> argument auto (metavar "INDEX" <> help "Index to delete, must be an integer")
                   shw      = SPrint  <$> argument auto (metavar "INDEX" <> help "Index to show, must be an integer")
                   mod       = SModify <$> index
                                        <*> (Serie <$> argument auto (metavar "INDEX") <*> tTitle <*> original <*> director <*> year
                                                        <*> episodesNumber <*> seasonsNumber <*> possession <*> watched)
                   imp       = SImport <$> importCSV
                   exp       = SExport <$> exportJSON
                   search    = SSearch <$> strArgument (metavar "FIELD" <> help "In what field (e.g. title/year) the search will be done") <*> strArgument (metavar "SEARCH" <> help "Thing to search for")

usage :: Parser Usage
usage = subparser $
       command "film"  (Films  <$> subCommandsFilms  `withInfo` "Work on Film list")
    <> command "serie" (Series <$> subCommandsSeries `withInfo` "Work on Serie list")
    <> command "init"  (pure Init `withInfo` "Initiate a Zamonia database")

runFilms :: FilmsCommand -> IO ()
runFilms (FAdd f)    = connection $ flip addWork f
runFilms (FDelete n) = connection $ flip delFilm n
runFilms (FPrint n)  = connection $ flip printFilm n
runFilms (FModify n f) = connection $ \c -> modWork c n f
runFilms FList       = connection $ listFilms >=> mapM_ (\(n, t) -> putStr $ printf "\ESC[1;32m%d\ESC[m %s\n" n t)
runFilms c           = putStrLn "Not implemented yet"

runSeries :: SeriesCommand -> IO ()
runSeries (SAdd f)    = connection $ \c -> addWork c f
runSeries (SDelete n) = connection $ \c -> delSerie c n
runSeries (SPrint n)  = connection $ flip printSerie n
runSeries (SModify n s) = connection $ \c -> modWork c n s
runSeries SList       = connection $ listSeries >=> mapM_ (\(n, t) -> putStr $ printf "\ESC[1;32m%d\ESC[m %s\n" n t)

runSeries c           = putStrLn "Not implemented yet"

main :: IO ()
main = do
    execution <- execParser (info (helper <*> usage)
            ( fullDesc
            <> progDesc "CLI personal library database"
            <> header "zamonia - a CLI personal library database written in haskell" ))
    case execution of
      Films c -> runFilms c
      Series c -> runSeries c
      Init -> connection $ \c -> execute_ c
                        "CREATE TABLE IF NOT EXISTS Films (IdF INTEGER PRIMARY KEY\
                                                        \ , Title         TEXT\
                                                        \ , OriginalTitle TEXT\
                                                        \ , Director      TEXT\
                                                        \ , Year          TEXT\
                                                        \ , Possession    TEXT\
                                                        \ , Watched       TEXT)"
                    >> execute_ c
                        "CREATE TABLE IF NOT EXISTS Series (IdS INTEGER PRIMARY KEY\
                                                        \ , Title         TEXT\
                                                        \ , OriginalTitle TEXT\
                                                        \ , Director      TEXT\
                                                        \ , Year          TEXT\
                                                        \ , EpisodeNumber TEXT\
                                                        \ , SeasonNumber  TEXT\
                                                        \ , Possession    TEXT\
                                                        \ , Watched       TEXT)"
