module FQuoter.Config.Config 
( Config
, accessDB 
, readConfig )
where

import Control.Monad.Trans
import Control.Applicative
import FQuoter.Serialize.Serialize
import Data.List.Split
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory
import System.Environment.XDG.BaseDir (getUserDataDir, getUserDataFile)

configFolder = "fquoter"

data Config = Config { currentDB :: FilePath
                     , currentTemplate :: String }

accessDB :: Config -> IO Connection
accessDB config = do
                    dbFolderPath <- getUserDataDir configFolder
                    let dbPath =  concat [dbFolderPath, "/", (currentDB config)]
                    dbExists <- doesFileExist dbPath
                    case dbExists of
                        True -> connectSqlite3 dbPath
                        False -> buildNewDB "schema.sql" dbPath >> connectSqlite3 dbPath

readConfig :: IO Config
readConfig = do path <- getUserDataDir configFolder
                createDirectoryIfMissing True path
                let pathConfig = (path ++ "/fquoter.cfg")
                fileExist <- doesFileExist pathConfig
                case fileExist of
                    True -> readConfig' pathConfig
                    False -> do
                        buildDefaultConfig pathConfig
                        readConfig' pathConfig

buildDefaultConfig :: String -> IO ()
buildDefaultConfig path = writeFile path content
    where defaultCurrent = "currentDB:defaultDB"
          defaultTemplate = "currentDisplay:"
                          ++ "[{maj}(%al), {maj,init}(%af)|{maj}(%an)]"
                          ++ "(%metaDate) {it}(%t). %metaPlace:%metaPublisher"
          content = unlines [defaultCurrent, defaultTemplate]

readConfig' :: FilePath -> IO Config
readConfig' path = do
                 f <- readFile path
                 let fItems = map (splitOn ":") . lines $ f
                 let currentDB = concat . tail . head $ fItems
                 let currentTemplate = concat . tail . last $ fItems
                 return $ Config currentDB currentTemplate
