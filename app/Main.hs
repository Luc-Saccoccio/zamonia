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
              <> value "todo.json"
              <> help "JSON file containing the lists")
          zCommand = subparser $
              cmd add  "add" "Add work to list"
           <> cmd del  "delete" "Delete first matching work from list"
           <> cmd mod  "modify" "Modify first matching work from list"
           <> cmd list "list" "List entries from list"
           <> cmd search "search" "Search keyword in list"
           <> command "init" (info (pure Init) (progDesc "Init a Zamonia database"))
          add       = Add <$> strArg "LIST"    <*> strArg "TITLE"
          del       = Delete <$> strArg "LIST" <*> strArg "INDEX"
          mod       = Modify <$> strArg "LIST" <*> strArg "INDEX"
          list      = List <$> strArg "LIST"
          search    = Search <$> strArg "LIST" <*> strArg "FIELD" <*> strArg "SEARCH"
          cmd p n d = command n (info (helper <*> p) (progDesc d))
          strArg n  = strArgument (metavar n)

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
      _    -> putStrLn "Other Action"
