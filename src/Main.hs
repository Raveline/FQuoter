import System.Environment
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad.Except
import Control.Monad.Reader

import FQuoter.Parser.ParserTypes
import FQuoter.Parser.Parser
import FQuoter.Serialize.Serialize
import FQuoter.Serialize.Shortcuts
import FQuoter.Serialize.SerializedTypes
import FQuoter.Serialize.Serialize

main :: IO ()
main = do
        args <- getArgs 
        case parseInput(unwords args) of 
            Left s -> print s
            Right c -> executeCommand c

executeCommand :: Action -> IO ()
executeCommand (Insert x) = insertAndDisplay x
executeCommand (FindWord w) = do db <- getDB
                                 result <- runExceptT $ runReaderT (process (searchWord w)) db
                                 case result of
                                    Left _ -> error "Should not happen. I think ?"
                                    Right qs -> displayQuotes qs
executeCommand (FindTags ts) = do db <- getDB
                                  result <- runExceptT $ runReaderT (process (searchTags ts)) db
                                  case result of
                                    Left _ -> error "Should not happen. I think ?"
                                    Right qs -> displayQuotes qs
executeCommand _ = putStrLn "Not implemented yet."

displayQuotes :: [SerializedType] -> IO ()
displayQuotes = mapM_ displayQuote
    where
        displayQuote :: SerializedType -> IO ()
        displayQuote (SQuote q) = print q
        displayQuotes _ = error "Not a quote. This should not happen !"

insertAndDisplay :: ParsedType -> IO ()
insertAndDisplay a = do db <- getDB 
                        result <- runExceptT $ runReaderT (process (insert a)) db
                        case result of
                            Right _ -> do 
                                        runReaderT (process commitAction) db
                                        putStrLn $ "Inserted " ++ (show a)
                            Left e -> displayException e >> runReaderT (process rollbackAction) db

displayException :: DBError -> IO ()
displayException (NonExistingDataError s) = putStrLn $ "The following author does not exist : " ++ s
displayException (AmbiguousDataError s) = putStrLn $ "Too many potential authors with your input. There are the possible choices : " ++ (unlines s)

handleError :: DBError -> IO ()
handleError = putStrLn . show

-- Obviously temporary
getDB :: IO Connection
getDB = connectSqlite3 "test.db"
