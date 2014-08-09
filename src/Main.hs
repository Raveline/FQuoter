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
import FQuoter.Templating.TemplateParser
import FQuoter.Templating.Display


main :: IO ()
main = do
        args <- getArgs
        config <- readConfig 
        case parseInput(unwords args) of 
            Left s -> print s
            Right c -> executeCommand config c

executeCommand :: Config -> Action -> IO ()
executeCommand c (Insert (Right x)) = insertAndDisplay c x
executeCommand c (Insert (Left nd)) = error "Not implemented."
executeCommand c (FindWord w) = do db <- accessDB c
                                   result <- runExceptT $ runReaderT (process (searchWord w)) db
                                   case result of
                                        Left _ -> error "Should not happen. I think ?"
                                        Right qs -> displayQuotes c qs
executeCommand c (FindTags ts) = do db <- accessDB c
                                    result <- runExceptT $ runReaderT (process (searchTags ts)) db
                                    case result of
                                        Left _ -> error "Should not happen. I think ?"
                                        Right qs -> displayQuotes c qs
executeCommand _ _ = putStrLn "Not implemented yet."

displayQuotes :: Config -> [SerializedType] -> IO ()
displayQuotes conf st 
    = case parser of
            Left err -> do putStrLn "Configuration file faulty."
                           putStrLn (show err)
                           putStrLn "Template cannot be parsed."
                           putStrLn "Falling back on default config."
                           displayQuotes buildDefaultConfig st
            Right x -> mapM_ (displayQuote x) st
    where
        parser = parseTemplate (currentTemplate conf)
        displayQuote template (SQuote q) = putStrLn $ readTree template q
        displayQuotes _ _ = error "Not a quote. This should not happen !"

insertAndDisplay :: Config -> ParsedType -> IO ()
insertAndDisplay c a = do db <- accessDB c
                          result <- runExceptT $ runReaderT (process (insert a)) db
                          case result of
                                Right _ -> do runReaderT (process commitAction) db
                                              putStrLn $ "Inserted " ++ (show a)
                                Left e -> print e >> runReaderT (process rollbackAction) db


handleError :: DBError -> IO ()
handleError = putStrLn . show
