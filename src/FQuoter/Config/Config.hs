module FQuoter.Config.Config where

data Config { currentDB :: Filepath }

writeConfig :: Config -> IO ()

readConfig :: String -> IO Config
readConfig path = do createDirectoryIfMissing True path
                     a
