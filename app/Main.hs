{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy     as BS
import           Database.SQLite.Simple
import qualified Data.Csv                 as C
import           Data.Semigroup           ((<>))
import           Data.Text                (Text)
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

-- TODO : Probably eliminate those two
cmd :: Parser a -> String -> String -> Mod CommandFields a
cmd p n d = command n (info (helper <*> p) (progDesc d))

-- strArg :: Data.String.IsString s => String -> String -> Parser s
strArg n h  = strArgument (metavar n <> help h)

tTitle :: Parser Text
tTitle = strOption ( long "title"
                <> short 't'
                <> metavar "TITLE"
                <> value ""
                <> help "Title" )

original :: Parser Text
original = strOption ( long "original"
                <> short 'o'
                <> metavar "TITLE"
                <> value ""
                <> help "Original Title")

director :: Parser Text
director = strOption ( long "director"
                <> short 'd'
                <> metavar "DIRECTOR"
                <> value ""
                <> help "Director")

year :: Parser Text
year = strOption ( long "year"
                <> short 'y'
                <> metavar "YEAR"
                <> value ""
                <> help "Year of release")

possession :: Parser Text
possession = strOption ( long "possession"
                <> short 'p'
                <> metavar "POSSESSION"
                <> value ""
                <> help "Weither your possess it or not, and how")

watched :: Parser Text
watched = strOption ( long "watched"
                <> short 'w'
                <> metavar "WATCHED"
                <> value ""
                <> help "Weither you watched it or not")

episodesNumber :: Parser Text
episodesNumber = strOption ( long "episodes-number"
                <> short 'n'
                <> metavar "NUMBER"
                <> value ""
                <> help "Number of episodes")

seasonsNumber :: Parser Text
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
              cmd add  "add" "Add work to list"
           <> cmd del  "delete" "Delete first matching work from list"
           <> cmd show "show" "Show informations about specified index"
           <> cmd mod  "modify" "Modify first matching work from list"
           <> cmd imp  "import" "Import a list"
           <> cmd exp  "export" "Export a list"
           <> command  "list" (info (pure FList) (progDesc "List entries from list"))
           <> cmd search "search" "Search keyword in list"
           <> cmd sort "sort" "Sort the list"
               where
                   add       = FAdd    <$> (Film <$> index <*> strArg "TITLE" "Title of the work you want to add"
                                                    <*> original <*> director <*> year <*> possession <*> watched)
                   del       = FDelete <$> argument auto (metavar "INDEX" <> help "Index to delete, must be an integer")
                   show      = FPrint  <$> argument auto (metavar "INDEX" <> help "Index to show, must be an integer")
                   mod       = FModify <$> argument auto (metavar "INDEX" <> help "Index to modify, must be an integer")
                                            <*> (Film <$> index <*> tTitle <*> original <*> director <*> year <*> possession <*> watched)
                   imp       = FImport <$> importCSV
                   exp       = FExport <$> exportJSON
                   search    = FSearch <$> strArg "FIELD" "In what field (e.g. title/year) the search will be done" <*> strArg "SEARCH" "Thing to search for"
                   sort      = FSort <$> strArg "FIELD" "Field to search"

subCommandsSeries :: Parser SeriesCommand
subCommandsSeries = subparser $
              cmd add  "add" "Add work to list"
           <> cmd del  "delete" "Delete first matching work from list"
           <> cmd show "show" "Show informations about specified index"
           <> cmd mod  "modify" "Modify first matching work from list"
           <> cmd imp  "import" "Import a list"
           <> cmd exp  "export" "Export a list"
           <> command  "list" (info (pure SList) (progDesc "List entries from list"))
           <> cmd search "search" "Search keyword in list"
           <> cmd sort "sort" "Sort the list"
               where
                   add       = SAdd    <$> (Serie <$> index <*> strArg "TITLE" "Title of the work you want to add"
                                                    <*> original <*> director <*> year <*> episodesNumber <*> seasonsNumber <*> possession <*> watched)
                   del       = SDelete <$> argument auto (metavar "INDEX" <> help "Index to delete, must be an integer")
                   show      = SPrint  <$> argument auto (metavar "INDEX" <> help "Index to show, must be an integer")
                   mod       = SModify <$> argument auto (metavar "INDEX" <> help "Index to modify, must be an integer")
                                                        <*> (Serie <$> index <*> tTitle <*> original <*> director <*> year
                                                        <*> episodesNumber <*> seasonsNumber <*> possession <*> watched)
                   imp       = SImport <$> importCSV
                   exp       = SExport <$> exportJSON
                   search    = SSearch <$> strArg "FIELD" "In what field (e.g. title/year) the search will be done" <*> strArg "SEARCH" "Thing to search for"
                   sort      = SSort <$> strArg "FIELD" "Field to search"

usage :: Parser Usage
usage = subparser $
       command "film"  (Films  <$> subCommandsFilms  `withInfo` "Work on Film list")
    <> command "serie" (Series <$> subCommandsSeries `withInfo` "Work on Serie list")
    <> command "init"  (pure Init `withInfo` "Initiate a Zamonia database")

-- readJson :: String -> IO (Either String BS.ByteString)
-- readJson file = do
--     exist <- doesFileExist file
--     if exist then
--              do
--                  content <- BS.readFile file
--                  return $ Right content
--     else
--         return . Left $ printf "List %s do not exist. Please create it" file

runFilms :: FilmsCommand -> IO ()
runFilms c = return ()

runSeries :: SeriesCommand -> IO ()
runSeries c = return ()

main :: IO ()
main = do
    execution <- execParser (info (helper <*> usage)
            ( fullDesc
            <> progDesc "CLI personal library database"
            <> header "zamonia - a CLI personal library database written in haskell" ))
    case execution of
      Films c -> runFilms c
      Series c -> runSeries c
      Init -> do
          conn <- open "zamonia.db"
          return ()
