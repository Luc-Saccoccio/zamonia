module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Semigroup       ((<>))
import           Options.Applicative
import           System.Directory
import           Zamonia

data Usage = Usage
    { file :: FilePath
    , zCmd :: Command }

usage :: Parser Usage
usage = Usage <$> file <*> zCommand
    where file = strOption
            (long "file"
              <> short 'f'
              <> metavar "FILE"
              <> value "todo.json" -- Modify or erase
              <> help "JSON file containing the lists")
          zCommand = subparser $
              cmd add  "add" "Add work to list"
           <> cmd del  "delete" "Delete first matching work from list"
           <> cmd mod  "modify" "Modify first matching work from list"
           <> cmd list "list" "List entries from list"
           <> cmd search "search" "Search keyword in list"
           <> command "init" (info (pure Init) (progDesc "Init a Zamonia database"))
          add       = Add    <$> strArg "LIST" "List to modify" <*> strArg "TITLE" "Title of the work you want to add"
          del       = Delete <$> strArg "LIST" "List to modify" <*> strArg "INDEX" "Index to delete"
          mod       = Modify <$> strArg "LIST" "List to modify" <*> strArg "INDEX" "Index to modify"
          list      = List   <$> strArg "LIST" "List to modify"
          search    = Search <$> strArg "LIST" "List to modify" <*> strArg "FIELD" "In what field (e.g. title/year) the search will be done" <*> strArg "SEARCH" "Thing to search for"
          cmd p n d = command n (info (helper <*> p) (progDesc d))
          strArg n h  = strArgument (metavar n <> help h)

-- To print list of work : sequence . listWork

main :: IO ()
main = do
    execution <- execParser (info (helper <*> usage)
            ( fullDesc
            <> progDesc "CLI personal library database"
            <> header "zamonia - a CLI personal library database written in haskell" ))
    case zCmd execution of
      Init -> do
          createDirectory "lists"
          writeFile "lists/series.json" ""
          writeFile "lists/films.json" ""
      Add l t      -> return ()
      Delete l i   -> return ()
      Modify l i   -> return ()
      List l       -> return ()
      Search l f s -> return ()
