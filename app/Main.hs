module Main where

import           Control.Monad        (void)
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.IntMap.Lazy     as I
import           Data.Semigroup       ((<>))
import           Options.Applicative
import           System.Directory
import           Text.Printf
import           Zamonia

data Usage = Init
           | Films Command
           | Series Command


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

subCommands :: Parser Command
subCommands = subparser $
              cmd add  "add" "Add work to list"
           <> cmd del  "delete" "Delete first matching work from list"
           <> cmd mod  "modify" "Modify first matching work from list"
           <> command  "list" (info (pure List) (progDesc "List entries from list"))
           <> cmd search "search" "Search keyword in list"
               where
                   add       = Add    <$> strArg "TITLE" "Title of the work you want to add"
                   del       = Delete <$> strArg "INDEX" "Index to delete"
                   mod       = Modify <$> strArg "INDEX" "Index to modify"
                   search    = Search <$> strArg "FIELD" "In what field (e.g. title/year) the search will be done" <*> strArg "SEARCH" "Thing to search for"
                   cmd p n d = command n (info (helper <*> p) (progDesc d))
                   strArg n h  = strArgument (metavar n <> help h)

usage :: Parser Usage
usage = subparser $
       command "film"  (Films <$> subCommands `withInfo` "Work on Film list")
    <> command "serie" (Series <$> subCommands `withInfo` "Work on Serie list")
    <> command "init"  (pure Init `withInfo` "Initiate a Zamonia database")

readJson :: String -> IO (Either String BS.ByteString)
readJson list = do
    let file = file ++ ".json"
    exist <- doesFileExist file
    if exist then
             return . Left $ printf "List %s do not exist. Please create it" file
    else
            do
                content <- BS.readFile file
                return $ Right content

runFilms :: Command -> IO ()
runFilms c = readJson "films" >>= either putStrLn (\rawList ->
    do
          let json = eitherDecode rawList :: Either String (I.IntMap Film)
          case json of
            Left s -> putStrLn s
            Right list -> case c of
                       Add _      -> return ()
                       Delete _   -> return ()
                       Modify _   -> return ()
                       Search _ _ -> return ()
                       List       -> void . sequence . listWork $ list)

-- To print list of work : sequence . listWork

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

    {-
        json <- BS.readFile todoFile
  let eLists = eitherDecode json :: Either String Lists
  case eLists of
    Left reason -> do
      putStr reason
      putStr "\n"
    Right lists -> do
      print lists
      let new_lists = addTask lists "second_list" pointless
      print "Added new todo task"
      print new_lists
      BS.writeFile todoFile (encodePretty new_lists)
-}
