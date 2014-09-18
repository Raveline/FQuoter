module FQuoter.Config.Config 
( Config (..)
, accessDB 
, buildDefaultConfig
, readConfig )
where

import FQuoter.Serialize.Serialize
import Data.List.Split
import Database.HDBC.Sqlite3
import System.Directory
import System.Environment.XDG.BaseDir (getUserDataDir)

configFolder = "fquoter"

data Config = Config { currentDB :: FilePath
                     , currentTemplate :: String }

accessDB :: Config -> IO Connection
accessDB config = do
                    dbFolderPath <- getUserDataDir configFolder
                    let dbPath =  concat [dbFolderPath, "/", currentDB config]
                    dbExists <- doesFileExist dbPath
                    if dbExists then connectSqlite3 dbPath
                                else buildNewDB "schema.sql" dbPath 
                                     >> connectSqlite3 dbPath

readConfig :: IO Config
readConfig = do path <- getUserDataDir configFolder
                createDirectoryIfMissing True path
                let pathConfig = path ++ "/fquoter.cfg"
                fileExist <- doesFileExist pathConfig
                if fileExist then readConfig' pathConfig
                             else do writeConfig buildDefaultConfig pathConfig
                                     readConfig' pathConfig

writeConfig :: Config -> FilePath -> IO ()
writeConfig conf path = writeFile path content
    where
        content = unlines [current, template]
        current = "currentDB:" ++ currentDB conf
        template = "currentDisplay:" ++ currentTemplate conf

buildDefaultConfig :: Config
buildDefaultConfig = Config defaultCurrent defaultTemplate
    where
        defaultCurrent = "defaultDB"
        defaultTemplate = "2[|{cap}%al, {init,cap}%af|{cap}%an|]"
            ++ " ?metaDate(%metaDate) ?{it}%t. %metaPlace : %metaPublisher."

readConfig' :: FilePath -> IO Config
readConfig' path = do
                 f <- readFile path
                 let fItems = map (splitOn ":") . lines $ f
                 let currentDB = concat . tail . head $ fItems
                 let currentTemplate = concat . tail . last $ fItems
                 return $ Config currentDB currentTemplate 
