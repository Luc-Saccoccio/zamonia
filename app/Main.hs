{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad          ((>=>))
import           Data.Semigroup         ((<>))
import           Database.SQLite.Simple
import           Options.Applicative
import           System.Directory       (createDirectoryIfMissing)
import           Text.Printf
import           Zamonia

data Usage = Init
           | Update
           | Films FilmsCommand
           | Series' SeriesCommand
           | Books BooksCommand

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

publisher :: Parser String
publisher = strOption ( long "publisher"
                <> short 'e'
                <> metavar "PUBLISHER"
                <> value ""
                <> help "Publisher/Editor")

author :: Parser String
author = strOption ( long "author"
                <> short 'a'
                <> metavar "AUTHOR"
                <> value ""
                <> help "Author")

isbn :: Parser String
isbn = strOption ( long "isbn"
            <> short 'i'
            <> metavar "ISBN"
            <> value ""
            <> help "IBSN")

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

sortNames :: Parser Sort
sortNames = flag Ids Names ( long "names"
            <> short 'n'
            <> help "Sort by names and not by ID")

sortWatched :: Parser Sort
sortWatched = flag Ids Done ( long "watched"
            <> short 'w'
            <> help "Sort by watching and not by ID")

sortRead :: Parser Sort
sortRead = flag Ids Done ( long "read"
            <> short 'r'
            <> help "Sort by watching and not by ID")

importCSV :: Parser FilePath
importCSV = strArgument (metavar "FILE" <> help "File to Import, must be a CSV")

importJSON :: Parser FilePath
importJSON = strArgument (metavar "FILE" <> help "File to Import, must be a JSON")

export :: Parser String
export = strArgument (metavar "FILE" <> help "Destination of the export")

subCommandsFilms :: Parser FilmsCommand
subCommandsFilms = subparser $
              command "add"    (info (helper <*> add) (progDesc "Add film to the database"))
           <> command "delete" (info (helper <*> del) (progDesc "Delete first matching film from database"))
           <> command "show"   (info (helper <*> shw) (progDesc "Show informations about specified index"))
           <> command "modify" (info (helper <*> mdf) (progDesc "Modify first matching film from database"))
           <> command "import-csv" (info (helper <*> imc) (progDesc "Import a list from CSV"))
           <> command "import-json" (info (helper <*> imj) (progDesc "Import a list from JSON"))
           <> command "export-csv" (info (helper <*> exc) (progDesc "Export a list to CSV"))
           <> command "export-json" (info (helper <*> exj) (progDesc "Export a list to JSON"))
           <> command "export-formatted" (info (helper <*> ext) (progDesc "Export a list to a list of formatted entries"))
           <> command "list"   (info (helper <*> lis) (progDesc "List entries from database"))
           <> command "purge"  (info (pure FPurge) (progDesc "Purge all rows from table in database"))
           <> command "search" (info (helper <*> search) (progDesc "Search keyword in database"))
               where
                   add       = FAdd    <$> (Film <$> index <*> strArgument (metavar "TITLE" <> help "Title of the film you want to add")
                                                    <*> original <*> director <*> year <*> possession <*> watched)
                   del       = FDelete <$> argument auto (metavar "INDEX" <> help "Index to delete, must be an integer")
                   shw       = FPrint  <$> argument auto (metavar "INDEX" <> help "Index to show, must be an integer")
                   mdf       = FModify <$> index
                                            <*> (Film <$> argument auto (metavar "INDEX" <> value (-1))
                                            <*> tTitle <*> original <*> director <*> year <*> possession <*> watched)
                   imc       = FImportCSV <$> importCSV
                   imj       = FImportJSON <$> importJSON
                   exc       = FExportCSV <$> export
                   exj       = FExportJSON <$> export
                   ext       = FExportFormatted <$> strArgument (metavar "FILE" <> help "Path of the template. Please refer to github.com/Luc-Saccoccio/zamonia for explanations") <*> export
                   lis       = FList <$> liftA2 (<~>) sortNames sortWatched
                   search    = FSearch <$> strArgument (metavar "FIELD" <> help "In what field (e.g. title/year) the search will be done") <*> strArgument (metavar "SEARCH" <> help "Thing to search for")

subCommandsSeries :: Parser SeriesCommand
subCommandsSeries = subparser $
              command "add"    (info (helper <*> add) (progDesc "Add a series to the database"))
           <> command "delete" (info (helper <*> del) (progDesc "Delete first matching series from database"))
           <> command "show"   (info (helper <*> shw) (progDesc "Show informations about specified index"))
           <> command "modify" (info (helper <*> mdf) (progDesc "Modify first matching film from database"))
           <> command "import-csv" (info (helper <*> imc) (progDesc "Import series from CSV"))
           <> command "import-json" (info (helper <*> imj) (progDesc "Import series from JSON"))
           <> command "export-csv" (info (helper <*> exc) (progDesc "Export a list to CSV"))
           <> command "export-json" (info (helper <*> exj) (progDesc "Export a list to JSON"))
           <> command "export-formatted" (info (helper <*> ext) (progDesc "Export a list to a list of formatted entries"))
           <> command "list"   (info (helper <*> lis) (progDesc "List entries from database"))
           <> command "purge"  (info (pure SPurge) (progDesc "Purge all rows from table in database"))
           <> command "search" (info (helper <*> search) (progDesc "Search keyword in database"))
               where
                   add       = SAdd    <$> (Series <$> index <*> strArgument (metavar "TITLE" <> help "Title of the series you want to add")
                                                    <*> original <*> director <*> year <*> episodesNumber <*> seasonsNumber <*> possession <*> watched)
                   del       = SDelete <$> argument auto (metavar "INDEX" <> help "Index to delete, must be an integer")
                   shw       = SPrint  <$> argument auto (metavar "INDEX" <> help "Index to show, must be an integer")
                   mdf       = SModify <$> index
                                        <*> (Series <$> argument auto (metavar "INDEX" <> value (-1)) <*> tTitle <*> original <*> director <*> year
                                                        <*> episodesNumber <*> seasonsNumber <*> possession <*> watched)
                   imc       = SImportCSV <$> importCSV
                   imj       = SImportJSON <$> importJSON
                   exc       = SExportCSV <$> export
                   exj       = SExportJSON <$> export
                   ext       = SExportFormatted <$> strArgument (metavar "FILE" <> help "Path of the template. Please refer to github.com/Luc-Saccoccio/zamonia for explanations") <*> export
                   lis       = SList <$> liftA2 (<~>) sortNames sortWatched
                   search    = SSearch <$> strArgument (metavar "FIELD" <> help "In what field (e.g. title/year) the search will be done") <*> strArgument (metavar "SEARCH" <> help "Thing to search for")

subCommandsBooks :: Parser BooksCommand
subCommandsBooks = subparser $
              command "add"    (info (helper <*> add) (progDesc "Add a book to the database"))
           <> command "delete" (info (helper <*> del) (progDesc "Delete first matching book from database"))
           <> command "show"   (info (helper <*> shw) (progDesc "Show informations about specified index"))
           <> command "modify" (info (helper <*> mdf) (progDesc "Modify first matching book from database"))
           <> command "import-csv" (info (helper <*> imc) (progDesc "Import books from CSV"))
           <> command "import-json" (info (helper <*> imj) (progDesc "Import books from JSON"))
           <> command "export-csv" (info (helper <*> exc) (progDesc "Export a list to CSV"))
           <> command "export-json" (info (helper <*> exj) (progDesc "Export a list to JSON"))
           <> command "export-formatted" (info (helper <*> ext) (progDesc "Export a list to a list of formatted entries"))
           <> command "list"   (info (helper <*> lis) (progDesc "List entries from database"))
           <> command "purge"  (info (pure BPurge) (progDesc "Purge all rows from table in database"))
           <> command "search" (info (helper <*> search) (progDesc "Search keyword in database"))
               where
                   add       = BAdd    <$> (Book <$> index <*> isbn <*> strArgument (metavar "TITLE" <> help "Title of the book you want to add")
                                                    <*> original <*> author <*> publisher <*> year <*> possession <*> watched)
                   del       = BDelete <$> argument auto (metavar "INDEX" <> help "Index to delete, must be an integer")
                   shw       = BPrint  <$> argument auto (metavar "INDEX" <> help "Index to show, must be an integer")
                   mdf       = BModify <$> index
                                        <*> (Book <$> argument auto (metavar "INDEX" <> value (-1)) <*> isbn <*> tTitle <*> original <*> author <*> publisher
                                                        <*> year <*> possession <*> watched)
                   imc       = BImportCSV <$> importCSV
                   imj       = BImportJSON <$> importJSON
                   exc       = BExportCSV <$> export
                   exj       = BExportJSON <$> export
                   ext       = BExportFormatted <$> strArgument (metavar "FILE" <> help "Path of the template. Please refer to github.com/Luc-Saccoccio/zamonia for explanations") <*> export
                   lis       = BList <$> liftA2 (<~>) sortNames sortRead
                   search    = BSearch <$> strArgument (metavar "FIELD" <> help "In what field (e.g. title/year) the search will be done") <*> strArgument (metavar "SEARCH" <> help "Thing to search for")

usage :: Parser Usage
usage = subparser $
       command "film"   (Films  <$> subCommandsFilms  `withInfo` "Work on Films list")
    <> command "series" (Series' <$> subCommandsSeries `withInfo` "Work on Series list")
    <> command "book"   (Books <$> subCommandsBooks `withInfo` "Work on Books list")
    <> command "update" (pure Update `withInfo` "Changes required if database was created before 0.2.0.0")
    <> command "init"  (pure Init `withInfo` "Initiate a Zamonia database")

runFilms :: FilmsCommand -> IO ()
runFilms (FAdd f)    = connection $ flip addWork f
runFilms (FDelete n) = connection $ flip delFilm n
runFilms (FPrint n)  = connection $ flip printFilm n
runFilms (FModify n f) = connection $ \c -> modWork c n f
runFilms (FImportJSON f) = connection $ flip importFilmsJSON f
runFilms (FImportCSV f) = connection $ flip importFilmsCSV f
runFilms (FExportJSON f) = connection $ flip exportFilmsJSON f
runFilms (FExportCSV f) = connection $ flip exportFilmsCSV f
runFilms (FExportFormatted t f) = connection $ filmsToFullFormatted t >=> writeFile f
runFilms (FList s)      = connection $ listFilms s >=> mapM_ (\(n, w, t) -> putStr $ printf "\ESC[1;32m%d\ESC[m\t\ESC[1;35m%s\ESC[m\t%s\n" n w t)
runFilms  FPurge = connection purgeFilms
runFilms _           = putStrLn "Not implemented yet"

runSeries :: SeriesCommand -> IO ()
runSeries (SAdd f)    = connection $ \c -> addWork c f
runSeries (SDelete n) = connection $ \c -> delSeries c n
runSeries (SPrint n)  = connection $ flip printSeries n
runSeries (SModify n s) = connection $ \c -> modWork c n s
runSeries (SImportJSON f) = connection $ flip importSeriesJSON f
runSeries (SImportCSV f) = connection $ flip importSeriesCSV f
runSeries (SExportJSON f) = connection $ flip exportSeriesJSON f
runSeries (SExportCSV f) = connection $ flip exportSeriesCSV f
runSeries (SExportFormatted t f) = connection $ seriesToFullFormatted t >=> writeFile f
runSeries (SList s)       = connection $ listSeries s >=> mapM_ (\(n, w, t) -> putStr $ printf "\ESC[1;32m%d\ESC[m\t\ESC[1;35m%s\ESC[m\t%s\n" n w t)
runSeries  SPurge = connection purgeSeries
runSeries _           = putStrLn "Not implemented yet"

runBooks :: BooksCommand -> IO ()
runBooks (BAdd f)    = connection $ \c -> addWork c f
runBooks (BDelete n) = connection $ \c -> delBook c n
runBooks (BPrint n)  = connection $ flip printBook n
runBooks (BModify n s) = connection $ \c -> modWork c n s
runBooks (BImportJSON f) = connection $ flip importBooksJSON f
runBooks (BImportCSV f) = connection $ flip importBooksCSV f
runBooks (BExportJSON f) = connection $ flip exportBooksJSON f
runBooks (BExportCSV f) = connection $ flip exportBooksCSV f
runBooks (BExportFormatted t f) = connection $ booksToFullFormatted t >=> writeFile f
runBooks (BList s)       = connection $ listBooks s >=> mapM_ (\(n, w, t) -> putStr $ printf "\ESC[1;32m%d\ESC[m\t\ESC[1;35m%s\ESC[m\t%s\n" n w t)
runBooks  BPurge = connection purgeBooks
runBooks _           = putStrLn "Not implemented yet"

main :: IO ()
main = do
    execution <- execParser (info (helper <*> usage)
            ( fullDesc
            <> progDesc "CLI personal library database"
            <> header "zamonia - a CLI personal library database written in haskell" ))
    case execution of
      Films c -> runFilms c
      Series' c -> runSeries c
      Books c -> runBooks c
      Update -> connection (\c -> execute_ c
                    "ALTER TABLE Films RENAME COLUMN Watched TO Done;"
                    >> execute_ c
                    "ALTER TABLE Films RENAME COLUMN Director TO Author;"
                    >> execute_ c
                    "ALTER TABLE Series RENAME COLUMN Watched TO Done;"
                    >> execute_ c
                    "ALTER TABLE Series RENAME COLUMN Director TO Author;")
      Init ->
          localLocation >>= createDirectoryIfMissing True
                    >> connection (\c -> execute_ c
                        "CREATE TABLE IF NOT EXISTS Films (IdF INTEGER PRIMARY KEY\
                                                        \ , Title         TEXT\
                                                        \ , OriginalTitle TEXT\
                                                        \ , Author        TEXT\
                                                        \ , Year          TEXT\
                                                        \ , Possession    TEXT\
                                                        \ , Done          TEXT)"
                    >> execute_ c
                        "CREATE TABLE IF NOT EXISTS Series (IdS INTEGER PRIMARY KEY\
                                                        \ , Title         TEXT\
                                                        \ , OriginalTitle TEXT\
                                                        \ , Author        TEXT\
                                                        \ , Year          TEXT\
                                                        \ , EpisodeNumber TEXT\
                                                        \ , SeasonNumber  TEXT\
                                                        \ , Possession    TEXT\
                                                        \ , Done          TEXT)"
                    >> execute_ c
                        "CREATE TABLE IF NOT EXISTS Books (IdB INTEGER PRIMARY KEY\
                                                       \ , ISBN          TEXT\
                                                       \ , Title         TEXT\
                                                       \ , OriginalTitle TEXT\
                                                       \ , Author        TEXT\
                                                       \ , Publisher     TEXT\
                                                       \ , Year          TEXT\
                                                       \ , Possession    TEXT\
                                                       \ , Done          TEXT)")
