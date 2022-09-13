{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Control.Monad          ((>=>))
import           Data.Proxy
import qualified Data.Text              as T
import qualified Data.Text.IO           as I
import qualified Data.Text.Lazy         as L
import           Database.SQLite.Simple
import           Options.Applicative
import           System.Directory       (createDirectoryIfMissing)
import           Zamonia
import qualified Zamonia.UI             (main)

data Command w =
            Add w
             | Delete Id
             | Print Id
             | Modify Id w
             | Search String String
             | ImportCSV FilePath
             | ImportJSON FilePath
             | ExportJSON FilePath
             | ExportCSV FilePath
             | ExportFormatted FilePath FilePath
             | List Sort
             | Purge

type FilmsCommand = Command Film
type SeriesCommand = Command Series
type BooksCommand = Command Book

data Usage = Init
           | TUI
           | Films FilmsCommand
           | Series' SeriesCommand
           | Books BooksCommand

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

index :: Parser Int
index = argument auto (metavar "INDEX" <> help "Index to add")

tTitle :: Parser T.Text
tTitle = strOption ( long "title"
                <> short 't'
                <> metavar "TITLE"
                <> value ""
                <> help "Title" )

original :: Parser T.Text
original = strOption ( long "original"
                <> short 'o'
                <> metavar "TITLE"
                <> value ""
                <> help "Original Title")

director :: Parser T.Text
director = strOption ( long "director"
                <> short 'd'
                <> metavar "DIRECTOR"
                <> value ""
                <> help "Director")

publisher :: Parser T.Text
publisher = strOption ( long "publisher"
                <> short 'e'
                <> metavar "PUBLISHER"
                <> value ""
                <> help "Publisher/Editor")

author :: Parser T.Text
author = strOption ( long "author"
                <> short 'a'
                <> metavar "AUTHOR"
                <> value ""
                <> help "Author")

isbn :: Parser T.Text
isbn = strOption ( long "isbn"
            <> short 'i'
            <> metavar "ISBN"
            <> value ""
            <> help "IBSN")

year :: Parser T.Text
year = strOption ( long "year"
                <> short 'y'
                <> metavar "YEAR"
                <> value ""
                <> help "Year of release")

possession :: Parser T.Text
possession = strOption ( long "possession"
                <> short 'p'
                <> metavar "POSSESSION"
                <> value ""
                <> help "Weither your possess it or not, and how")

watched :: Parser T.Text
watched = strOption ( long "watched"
                <> short 'w'
                <> metavar "WATCHED"
                <> value ""
                <> help "Weither you watched it or not")

episodesNumber :: Parser T.Text
episodesNumber = strOption ( long "episodes-number"
                <> short 'n'
                <> metavar "NUMBER"
                <> value ""
                <> help "Number of episodes")

seasonsNumber :: Parser T.Text
seasonsNumber = strOption ( long "seasons-number"
                <> short 's'
                <> metavar "NUMBER"
                <> value ""
                <> help "Number of seasons")

sortNames :: Parser Sort
sortNames = flag Ids Names ( long "names"
            <> short 'n'
            <> help "Sort by names and not by ID")

sortStatus :: Parser Sort
sortStatus = flag Ids Done ( long "read"
            <> short 'r'
            <> help "Sort by status and not by ID")

importFileCSV :: Parser FilePath
importFileCSV = strArgument (metavar "FILE" <> help "File to Import, must be a CSV")

importFileJSON :: Parser FilePath
importFileJSON = strArgument (metavar "FILE" <> help "File to Import, must be a JSON")

export :: Parser String
export = strArgument (metavar "FILE" <> help "Destination of the export")

subCommandsBase :: (Int -> Id) -> Mod CommandFields (Command w)
subCommandsBase cons =
     command "delete" (info (helper <*> del) (progDesc "Delete first matching book from database"))
  <> command "show"   (info (helper <*> shw) (progDesc "Show informations about specified index"))
  <> command "import-csv" (info (helper <*> imc) (progDesc "Import entries from CSV"))
  <> command "import-json" (info (helper <*> imj) (progDesc "Import entries from JSON"))
  <> command "export-csv" (info (helper <*> exc) (progDesc "Export a list to CSV"))
  <> command "export-json" (info (helper <*> exj) (progDesc "Export a list to JSON"))
  <> command "export-formatted" (info (helper <*> ext) (progDesc "Export a list to a list of formatted entries"))
  <> command "list"   (info (helper <*> lis) (progDesc "List entries from database"))
  <> command "purge"  (info (pure Purge) (progDesc "Purge all rows from table in database"))
  <> command "search" (info (helper <*> search) (progDesc "Search keyword in database"))
    where
      del       = Delete . cons <$> argument auto (metavar "INDEX" <> help "Index to delete, must be an integer")
      shw       = Print . cons <$> argument auto (metavar "INDEX" <> help "Index to show, must be an integer")
      imc       = ImportCSV <$> importFileCSV
      imj       = ImportJSON <$> importFileJSON
      exc       = ExportCSV <$> export
      exj       = ExportJSON <$> export
      ext       = ExportFormatted <$> strArgument (metavar "FILE" <> help "Path of the template. Please refer to github.com/Luc-Saccoccio/zamonia for explanations") <*> export
      lis       = List <$> liftA2 (<~>) sortNames sortStatus
      search    = Search <$> strArgument (metavar "FIELD" <> help "In what field (e.g. title/year) the search will be done") <*> strArgument (metavar "SEARCH" <> help "Thing to search for")

subCommandsFilms :: Parser FilmsCommand
subCommandsFilms = subparser $
              subCommandsBase IdF
           <> command "add"    (info (helper <*> add) (progDesc "Add film to the database"))
           <> command "modify" (info (helper <*> mdf) (progDesc "Modify first matching film from database"))
               where
                   add       = Add    <$> (Film <$> index <*> strArgument (metavar "TITLE" <> help "Title of the film you want to add")
                                                   <*> original <*> director <*> year <*> possession <*> watched)
                   mdf       = Modify <$> (IdF <$> index)
                                           <*> (Film <$> argument auto (metavar "INDEX" <> value (-1))
                                           <*> tTitle <*> original <*> director <*> year <*> possession <*> watched)

subCommandsSeries :: Parser SeriesCommand
subCommandsSeries = subparser $
              subCommandsBase IdS
           <> command "add"    (info (helper <*> add) (progDesc "Add a series to the database"))
           <> command "modify" (info (helper <*> mdf) (progDesc "Modify first matching film from database"))
               where
                   add       = Add    <$> (Series <$> index <*> strArgument (metavar "TITLE" <> help "Title of the series you want to add")
                                                   <*> original <*> director <*> year <*> episodesNumber <*> seasonsNumber <*> possession <*> watched)
                   mdf       = Modify <$> (IdS <$> index)
                                       <*> (Series <$> argument auto (metavar "INDEX" <> value (-1)) <*> tTitle <*> original <*> director <*> year
                                                       <*> episodesNumber <*> seasonsNumber <*> possession <*> watched)

subCommandsBooks :: Parser BooksCommand
subCommandsBooks = subparser $
              subCommandsBase IdB
           <> command "modify" (info (helper <*> mdf) (progDesc "Modify first matching book from database"))
           <> command "add"    (info (helper <*> add) (progDesc "Add a book to the database"))
               where
                   add       = Add    <$> (Book <$> index <*> isbn <*> strArgument (metavar "TITLE" <> help "Title of the book you want to add")
                                                   <*> original <*> author <*> publisher <*> year <*> possession <*> watched)
                   mdf       = Modify <$> (IdB <$> index)
                                       <*> (Book <$> index <*> isbn <*> tTitle <*> original <*> author <*> publisher
                                                       <*> year <*> possession <*> watched)

usage :: Parser Usage
usage = subparser $
       command "film"   (Films  <$> subCommandsFilms  `withInfo` "Work on Films list")
    <> command "series" (Series' <$> subCommandsSeries `withInfo` "Work on Series list")
    <> command "book"   (Books <$> subCommandsBooks `withInfo` "Work on Books list")
    <> command "init"   (pure Init `withInfo` "Initiate a Zamonia database")
    <> command "tui"    (pure TUI `withInfo` "Open the TUI")


run :: Cons work => proxy work -> Command work -> IO ()
run _ (Delete n) = connection $ flip delWork n
run _ (Add f) = connection $ flip addWork f
run _ (Modify n s) = connection $ \c -> modWork c n s
run (_ :: proxy work) (Print n) = connection $ \c -> fetchWork c n >>= printWork @work
run (_ :: proxy work) (ImportJSON f) = connection $ \c -> importJSON (Proxy @work) c f
run (_ :: proxy work) (ImportCSV  f) = connection $ \c -> importCSV (Proxy @work) c f
run (_ :: proxy work) (ExportJSON f) = connection $ \c -> exportJSON (Proxy @work) c f
run (_ :: proxy work) (ExportCSV  f) = connection $ \c -> exportCSV (Proxy @work) c f
run (_ :: proxy work) (ExportFormatted t f) =
  connection $ allToFullFormatted (Proxy @work) t >=> I.writeFile f . L.toStrict
run _ _ = putStrLn "Not implemented yet"


{- TODO
runFilms :: FilmsCommand -> IO ()
runFilms (List s) = connection $ listFilms s >=> mapM_
  (\(n, w, t) ->
    putStr $ printf "\ESC[1;32m%d\ESC[m\t\ESC[1;35m%s\ESC[m\t%s\n" n w t
  )
runSeries :: SeriesCommand -> IO ()
runSeries (List s) = connection $ listSeries s >=> mapM_
  (\(n, w, t) ->
    putStr $ printf "\ESC[1;32m%d\ESC[m\t\ESC[1;35m%s\ESC[m\t%s\n" n w t
  )
runBooks :: BooksCommand -> IO ()
runBooks (List s) = connection $ listBooks s >=> mapM_
  (\(n, w, t) ->
    putStr $ printf "\ESC[1;32m%d\ESC[m\t\ESC[1;35m%s\ESC[m\t%s\n" n w t
  )

runFilms Purge          = connection purgeFilms
runSeries Purge          = connection purgeSeries
runBooks Purge          = connection purgeBooks
-}

main :: IO ()
main = do
    execution <- execParser (info (helper <*> usage)
            ( fullDesc
            <> progDesc "CLI personal library database"
            <> header "zamonia - a CLI personal library database written in haskell" ))
    case execution of
      Films c -> run (Proxy @Film) c
      Series' c -> run (Proxy @Series) c
      Books c -> run (Proxy @Book) c
      TUI -> Zamonia.UI.main
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
                                                       \ , Done          TEXT)")
