import Database.HDBC hiding (execute)
import System.Environment
import System.Console.Haskeline
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe

import FQuoter.Display
import FQuoter.Quote
import FQuoter.Actions
import FQuoter.Parser.Parser
import FQuoter.Parser.ParserTypes
import FQuoter.Serialize.Shortcuts
import FQuoter.Serialize.SerializedTypes
import FQuoter.Serialize.Serialize
import FQuoter.Config.Config
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
executeCommand c (Insert (Right x)) = insertAndDisplay c x
executeCommand c (Insert (Left nd)) = shellForNotDefined nd >>= executeCommand c . Insert . Right
executeCommand c (FindWord w) = executeSearch c (searchWord w)
executeCommand c (FindTags ts) = executeSearch c (searchTags ts)
executeCommand c (Remove t n) = deleteAndConfirm c t n
executeCommand c (Updating t s u p) = handleUpdate c t s u p
executeCommand _ _ = outputStrLn "Not implemented yet."

handleUpdate :: Config -> DBType -> String -> Update -> TypeProperty -> InputT IO ()
handleUpdate c DBAuthor s u p = updateAndConfirm c DBAuthor s u p
handleUpdate c DBSource s u p@(ModifySource (SourceTitle _)) = updateAndConfirm c DBSource s u p
handleUpdate _ _ _ _ _ = error "Not implemented yet."

updateAndConfirm :: Config -> DBType -> String -> Update -> TypeProperty -> InputT IO ()
updateAndConfirm c t s u p = do res <- liftIO $ execute c $ updateMainProperty t u p s 
                                commitAndSay (snd res) "Updated properly."

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
                                                    then commitAndSay (snd result) "Deleted."
                                                    else rollbackAndSay (snd result) "Cancelled."


confirmed = do c <- getInputChar ""
               let c' = fromMaybe 'n' c
               return (c' == 'y')

executeSearch c f = do result <- liftIO $ execute c f
                       case fst result of
                            Left _ -> error "Should not happen. I think ?"
                            Right qs -> displayQuotes c qs

insertAndDisplay :: Config -> ParsedType -> InputT IO ()
insertAndDisplay c a 
    = do result <- liftIO $ execute c (insert a)
         case fst result of
             Right _ -> do commitAndSay (snd result) ("Inserted " ++ show a)
             Left e -> outputStrLn (show e)
                       >> rollbackAndSay (snd result) ("Could not insert " ++ show a)

handleError :: DBError -> IO ()
handleError = print

rollbackAndSay :: (IConnection c) => c -> String -> InputT IO ()
rollbackAndSay c s = runReaderT (process rollbackAction) c
                     >> outputStrLn s

commitAndSay :: (IConnection c) => c -> String -> InputT IO ()
commitAndSay c s = runReaderT (process commitAction) c
                   >> outputStrLn s
