import System.Environment
import Database.HDBC
import Control.Monad.Except
import Control.Monad.Reader

import FQuoter.Parser.ParserTypes
import FQuoter.Parser.Parser
import FQuoter.Serialize.Serialize
import FQuoter.Serialize.Shortcuts
import FQuoter.Serialize.SerializedTypes
import FQuoter.Serialize.Serialize
import FQuoter.Config.Config


main :: IO ()
main = do
        args <- getArgs
        config <- readConfig 
        case parseInput(unwords args) of 
            Left s -> print s
            Right c -> executeCommand config c

executeCommand :: Config -> Action -> IO ()
executeCommand c (Insert x) = insertAndDisplay c x
executeCommand c (FindWord w) = do db <- accessDB c
                                   result <- runExceptT $ runReaderT (process (searchWord w)) db
                                   case result of
                                        Left _ -> error "Should not happen. I think ?"
                                        Right qs -> displayQuotes qs
executeCommand c (FindTags ts) = do db <- accessDB c
                                    result <- runExceptT $ runReaderT (process (searchTags ts)) db
                                    case result of
                                        Left _ -> error "Should not happen. I think ?"
                                        Right qs -> displayQuotes qs
executeCommand _ _ = putStrLn "Not implemented yet."

displayQuotes :: [SerializedType] -> IO ()
displayQuotes = mapM_ displayQuote
    where
        displayQuote :: SerializedType -> IO ()
        displayQuote (SQuote q) = print q
        displayQuotes _ = error "Not a quote. This should not happen !"

insertAndDisplay :: Config -> ParsedType -> IO ()
insertAndDisplay c a = do db <- accessDB c
                          result <- runExceptT $ runReaderT (process (insert a)) db
                          case result of
                                Right _ -> do runReaderT (process commitAction) db
                                              putStrLn $ "Inserted " ++ (show a)
                                Left e -> print e >> runReaderT (process rollbackAction) db


handleError :: DBError -> IO ()
handleError = putStrLn . show
