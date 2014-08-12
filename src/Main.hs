
import Data.List hiding (insert)
import Data.Maybe
import System.Environment
import System.Console.Haskeline
import Control.Monad.Except
import Control.Monad.Reader

import FQuoter.Quote
import FQuoter.Parser.ParserTypes
import FQuoter.Parser.Parser
import FQuoter.Serialize.Shortcuts
import FQuoter.Serialize.SerializedTypes
import FQuoter.Serialize.Serialize
import FQuoter.Config.Config
import FQuoter.Templating.TemplateParser
import FQuoter.Templating.Display
import FQuoter.Repl


main :: IO ()
main = runInputT defaultSettings interpreter

interpreter :: InputT IO ()
interpreter = do args <- liftIO getArgs
                 config <- liftIO readConfig 
                 case parseInput(unwords args) of 
                    Left s -> outputStrLn $ show s
                    Right c -> executeCommand config c

executeCommand :: Config -> Action -> InputT IO ()
executeCommand c (Insert (Right x)) = liftIO $ insertAndDisplay c x
executeCommand c (Insert (Left nd)) = shellForNotDefined nd >>= executeCommand c . Insert . Right
executeCommand c (FindWord w) = executeSearch c (searchWord w)
executeCommand c (FindTags ts) = executeSearch c (searchTags ts)
executeCommand _ _ = outputStrLn "Not implemented yet."

executeSearch c f = do db <- liftIO $ accessDB c
                       result <- liftIO $ runExceptT $ runReaderT (process f) db
                       case result of
                            Left _ -> error "Should not happen. I think ?"
                            Right qs -> displayQuotes c qs

displayQuotes :: Config -> [SerializedType] -> InputT IO ()
displayQuotes conf st 
    = case parser of
            Left err -> do let msg = ["Configuration file faulty."
                                     ,show err
                                     ,"Template cannot be parsed."
                                     ,"Falling back on default config."]
                           mapM_ outputStrLn msg
                           displayQuotes buildDefaultConfig st
            Right x -> mapM_ (displayQuote x) st
    where
        parser = parseTemplate (currentTemplate conf)

displayQuote template (SQuote q) = mapM_ outputStrLn $ catMaybes displayed
    where
        displayed = [Just line
                    , Just $ "\"" ++ content q ++ "\""
                    , Just ""
                    , Just $ readTree template q
                    , comment q 
                    , Just ""
                    , outputTagsArray $ tags q
                    , Just line]
        displayQuotes _ _ = error "Not a quote. This should not happen !"
        outputTagsArray [] = Nothing
        outputTagsArray xs = Just $ "Tags : " ++ intercalate "," xs

insertAndDisplay :: Config -> ParsedType -> IO ()
insertAndDisplay c a = do db <- accessDB c
                          result <- runExceptT $ runReaderT (process (insert a)) db
                          case result of
                                Right _ -> do runReaderT (process commitAction) db
                                              putStrLn $ "Inserted " ++ show a
                                Left e -> print e >> runReaderT (process rollbackAction) db


handleError :: DBError -> IO ()
handleError = print

line = replicate 80 '-' 
outputMaybe Nothing = return ()
outputMaybe (Just s) = outputStrLn s
