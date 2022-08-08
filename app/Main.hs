{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Control.Monad          ((>=>))
import           Data.Proxy
import           Data.String            (IsString)
import qualified Data.Text.IO           as I
import qualified Data.Text.Lazy         as L
import           Database.SQLite.Simple
import           Options.Applicative
import           System.Directory       (createDirectoryIfMissing)
import           Text.Printf
import qualified Zamonia.UI (main)

{- data Usage = Init
           | Films FilmsCommand
           | Series' SeriesCommand
           | Books BooksCommand

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

index :: Parser Int
index = argument auto (metavar "INDEX" <> help "Index to add")

tTitle :: IsString p => Parser p
tTitle = strOption ( long "title"
                <> short 't'
                <> metavar "TITLE"
                <> value ""
                <> help "Title" )

original :: IsString p => Parser p
original = strOption ( long "original"
                <> short 'o'
                <> metavar "TITLE"
                <> value ""
                <> help "Original Title")

director :: IsString p => Parser p
director = strOption ( long "director"
                <> short 'd'
                <> metavar "DIRECTOR"
                <> value ""
                <> help "Director")

publisher :: IsString p => Parser p
publisher = strOption ( long "publisher"
                <> short 'e'
                <> metavar "PUBLISHER"
                <> value ""
                <> help "Publisher/Editor")

author :: IsString p => Parser p
author = strOption ( long "author"
                <> short 'a'
                <> metavar "AUTHOR"
                <> value ""
                <> help "Author")

isbn :: IsString p => Parser p
isbn = strOption ( long "isbn"
            <> short 'i'
            <> metavar "ISBN"
            <> value ""
            <> help "IBSN")

year :: IsString p => Parser p
year = strOption ( long "year"
                <> short 'y'
                <> metavar "YEAR"
                <> value ""
                <> help "Year of release")

possession :: IsString p => Parser p
possession = strOption ( long "possession"
                <> short 'p'
                <> metavar "POSSESSION"
                <> value ""
                <> help "Weither your possess it or not, and how")

watched :: IsString p => Parser p
watched = strOption ( long "watched"
                <> short 'w'
                <> metavar "WATCHED"
                <> value ""
                <> help "Weither you watched it or not")

episodesNumber :: IsString p => Parser p
episodesNumber = strOption ( long "episodes-number"
                <> short 'n'
                <> metavar "NUMBER"
                <> value ""
                <> help "Number of episodes")

seasonsNumber :: IsString p => Parser p
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

importFileCSV :: Parser FilePath
importFileCSV = strArgument (metavar "FILE" <> help "File to Import, must be a CSV")

importFileJSON :: Parser FilePath
importFileJSON = strArgument (metavar "FILE" <> help "File to Import, must be a JSON")

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
                   imc       = FImportCSV <$> importFileCSV
                   imj       = FImportJSON <$> importFileJSON
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
                   imc       = SImportCSV <$> importFileCSV
                   imj       = SImportJSON <$> importFileJSON
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
                                        <*> (Book <$> index <*> isbn <*> tTitle <*> original <*> author <*> publisher
                                                        <*> year <*> possession <*> watched)
                   imc       = BImportCSV <$> importFileCSV
                   imj       = BImportJSON <$> importFileJSON
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
    <> command "init"  (pure Init `withInfo` "Initiate a Zamonia database")

runFilms :: FilmsCommand -> IO ()
runFilms (FAdd    f    ) = connection $ flip addWork f
runFilms (FDelete n    ) = connection $ flip delFilm n
-- runFilms (FPrint  n    ) = connection $ \c -> fetchWork c n >>= printWork
runFilms (FModify n f  ) = connection $ \c -> modWork c n f
runFilms (FImportJSON f) = connection $ \c -> importJSON (Proxy @Film) c f
runFilms (FImportCSV  f) = connection $ \c -> importCSV (Proxy @Film) c f
runFilms (FExportJSON f) = connection $ \c -> exportJSON (Proxy @Film) c f
runFilms (FExportCSV  f) = connection $ \c -> exportCSV (Proxy @Film) c f
runFilms FPurge          = connection purgeFilms
runFilms (FExportFormatted t f) =
  connection $ allToFullFormatted (Proxy @Film) t >=> I.writeFile f . L.toStrict
runFilms (FList s) = connection $ listFilms s >=> mapM_
  (\(n, w, t) ->
    putStr $ printf "\ESC[1;32m%d\ESC[m\t\ESC[1;35m%s\ESC[m\t%s\n" n w t
  )
runFilms _ = putStrLn "Not implemented yet"

runSeries :: SeriesCommand -> IO ()
runSeries (SAdd    f    ) = connection $ \c -> addWork c f
runSeries (SDelete n    ) = connection $ \c -> delSeries c n
-- runSeries (SPrint  n    ) = connection $ \c -> fetchWork c n >>= printWork
runSeries (SModify n s  ) = connection $ \c -> modWork c n s
runSeries (SImportJSON f) = connection $ \c -> importJSON (Proxy @Series) c f
runSeries (SImportCSV  f) = connection $ \c -> importCSV (Proxy @Series) c f
runSeries (SExportJSON f) = connection $ \c -> exportJSON (Proxy @Series) c f
runSeries (SExportCSV  f) = connection $ \c -> exportCSV (Proxy @Series) c f
runSeries SPurge          = connection purgeSeries
runSeries (SExportFormatted t f) =
    connection $ allToFullFormatted (Proxy @Series) t >=> I.writeFile f . L.toStrict
runSeries (SList s) = connection $ listSeries s >=> mapM_
  (\(n, w, t) ->
    putStr $ printf "\ESC[1;32m%d\ESC[m\t\ESC[1;35m%s\ESC[m\t%s\n" n w t
  )
runSeries _ = putStrLn "Not implemented yet"

runBooks :: BooksCommand -> IO ()
runBooks (BAdd    f    ) = connection $ \c -> addWork c f
runBooks (BDelete n    ) = connection $ \c -> delBook c n
-- runBooks (BPrint  n    ) = connection $ \c -> fetchWork c n >>= printWork
runBooks (BModify n s  ) = connection $ \c -> modWork c n s
runBooks (BImportJSON f) = connection $ \c -> importJSON (Proxy @Book) c f
runBooks (BImportCSV  f) = connection $ \c -> importCSV (Proxy @Book) c f
runBooks (BExportJSON f) = connection $ \c -> exportJSON (Proxy @Book) c f
runBooks (BExportCSV  f) = connection $ \c -> exportCSV (Proxy @Book) c f
runBooks BPurge          = connection purgeBooks
runBooks (BExportFormatted t f) =
  connection $ allToFullFormatted (Proxy @Book) t >=> I.writeFile f . L.toStrict
runBooks (BList s) = connection $ listBooks s >=> mapM_
  (\(n, w, t) ->
    putStr $ printf "\ESC[1;32m%d\ESC[m\t\ESC[1;35m%s\ESC[m\t%s\n" n w t
  )
runBooks _ = putStrLn "Not implemented yet"

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
      Init ->
          localLocation >>= createDirectoryIfMissing True
          >> booksLocation >>= createDirectoryIfMissing True
          >> seriesLocation >>= createDirectoryIfMissing True
          >> filmsLocation >>= createDirectoryIfMissing True
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
                        "CREATE TABLE Books (IdB INTEGER PRIMARY KEY\
                                                       \ , ISBN          TEXT\
                                                       \ , Title         TEXT\
                                                       \ , OriginalTitle TEXT\
                                                       \ , Author        TEXT\
                                                       \ , Publisher     TEXT\
                                                       \ , Year          TEXT\
                                                       \ , Possession    TEXT\
                                                       \ , Done          TEXT)") -}
main :: IO ()
main = Zamonia.UI.main
