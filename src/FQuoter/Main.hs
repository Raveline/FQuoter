
import Data.List hiding (insert, delete)
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
executeCommand c (Remove t n) = deleteAndConfirm c t n
executeCommand _ _ = outputStrLn "Not implemented yet."

execute c f = do db <- accessDB c
                 res <- runExceptT $ runReaderT (process f) db
                 return (res, db)

deleteAndConfirm :: Config -> DBType -> String -> InputT IO ()
deleteAndConfirm c t n = do result <- liftIO $ execute c (remove t n)
                            case fst result of
                                Left err -> do let msg = ["Could not delete."
                                                         ,show err]
                                               mapM_ outputStrLn msg
                                Right res -> do let msg = ["Are you sure you want to delete (y/N) :"
                                                          ,show res]
                                                mapM_ outputStrLn msg
                                                conf <- confirmed
                                                if conf
                                                    then runReaderT (process commitAction) (snd result)
                                                    else runReaderT (process rollbackAction) (snd result)

confirmed = do c <- getInputChar ""
               let c' = fromMaybe 'n' c
               return (c' == 'y')

executeSearch c f = do result <- liftIO $ execute c f
                       case fst result of
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
insertAndDisplay c a 
    = do result <- liftIO $ execute c (insert a)
         case fst result of
             Right _ -> do runReaderT (process commitAction) (snd result)
                           putStrLn $ "Inserted " ++ show a
             Left e -> print e 
                       >> runReaderT (process rollbackAction) (snd result)


handleError :: DBError -> IO ()
handleError = print

line = replicate 80 '-' 
outputMaybe Nothing = return ()
outputMaybe (Just s) = outputStrLn s
