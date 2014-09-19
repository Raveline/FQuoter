module FQuoter.Commands
(executeCommand)
where

import Database.HDBC hiding (execute)
import Database.HDBC.Sqlite3
import System.Console.Haskeline
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import Text.ParserCombinators.Parsec

import FQuoter.Display
import FQuoter.Actions
import FQuoter.Parser.Parser
import FQuoter.Parser.ParserTypes
import FQuoter.Serialize.Shortcuts
import FQuoter.Serialize.SerializedTypes
import FQuoter.Serialize.Serialize hiding (Delete)
import FQuoter.Config.Config
import FQuoter.Repl.ReplUtils
import FQuoter.Repl.ReplForms

-- The main interpreter from Quoter. With a given configuration,
-- this function will call the various subsystems necessary to
-- perform the user's request.
executeCommand :: Config -> Action -> InputT IO ()
executeCommand c (Insert (Right x)) = insertAndDisplay c x
executeCommand c (Insert (Left nd)) = shellForNotDefined nd 
                                      >>= executeCommand c . Insert . Right
executeCommand c (FindWord w) = executeSearch c (searchWord w)
executeCommand c (FindTags ts) = executeSearch c (searchTags ts)
executeCommand c (Remove t n) = deleteAndConfirm c t n
executeCommand c (Updating t s u p) = handleUpdate c t s u p
executeCommand c (Shell) = shellMode c

-- Updating something remains the most unelegantly expressed task.
-- We have to divide between updating main qualities of the object,
-- i.e. things that are not stored through a join in the database,
-- and the updation that will need to modify those join tables.
-- The general idea here is to find the proper methods from
-- Serialize.Shortcuts.
handleUpdate :: Config -> DBType -> String -> Update -> TypeProperty -> InputT IO ()
handleUpdate c DBAuthor s u p = updateAndConfirm c (updateMainProperty DBAuthor u p s)
handleUpdate c DBSource s u p@(ModifySource (SourceTitle _)) 
    = updateAndConfirm c (updateMainProperty DBSource u p s)
handleUpdate c DBSource s Add p@(ModifySource _)
    = updateAndConfirm c (updateAddAssociation DBSource p s)
handleUpdate c DBSource s Delete p = updateAndConfirm c (updateRemoveAssociation DBSource p s)
handleUpdate _ _ _ _ _ = error "Not implemented yet."

-- Perform an update action, commit and inform the user
-- everything went fine.
updateAndConfirm :: Config -> FalliableSerialization Connection IO a -> InputT IO()
updateAndConfirm c f = do res <- liftIO $ execute c f 
                          commitAndSay (snd res) "Updated properly."

-- Execute is here to call a function with side effect through
-- a monadic stack.
-- First, it opens a database.
-- Then it prepares a stack with Exception, Reader (to access the DB)
-- and our Free Monad (see Serialize.Serialize).
-- It then returns the result of the side effect, and the handler to
-- the database so it can be easily commited or rolled-back.
execute :: Config 
           -> FalliableSerialization Connection IO a 
           -> IO (Either DBError a, Connection)
execute c f = do db <- accessDB c
                 res <- runExceptT $ runReaderT (process f) db
                 return (res, db)

-- When deleting something, we want to make sure the user knows what
-- he or she's doing. We first do the action, then ask for a
-- confirmation. If the user confirms, we commit, else, we rollback.
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

-- Simple utility function to read a "y/n" choice from the user,
-- using Haskeline.
confirmed :: InputT IO Bool
confirmed = do c <- getInputChar ""
               let c' = fromMaybe 'n' c
               return (c' == 'y')

executeSearch :: Config 
                 -> FalliableSerialization Connection IO [SerializedType] 
                 -> InputT IO ()
executeSearch c f = do result <- liftIO $ execute c f
                       case fst result of
                            Left _ -> error "Should not happen. I think ?"
                            Right qs -> displayQuotes c qs

insertAndDisplay :: Config -> ParsedType -> InputT IO ()
insertAndDisplay c a 
    = do result <- liftIO $ execute c (insert a)
         case fst result of
             Right _ -> commitAndSay (snd result) ("Inserted " ++ show a)
             Left e -> outputStrLn (show e)
                       >> rollbackAndSay (snd result) ("Could not insert " ++ show a)

rollbackAndSay :: (IConnection c) => c -> String -> InputT IO ()
rollbackAndSay c s = runReaderT (process rollbackAction) c
                     >> outputStrLn s

commitAndSay :: (IConnection c) => c -> String -> InputT IO ()
commitAndSay c s = runReaderT (process commitAction) c
                   >> outputStrLn s

shellMode :: Config -> InputT IO ()
shellMode c = liftM parseInput (getInputLine' prompt)
              >>= interpretationResult c
              >> shellMode c

interpretationResult :: Config -> Either ParseError Action -> InputT IO()
interpretationResult _ (Left s) = outputStrLn $ show s
interpretationResult _ (Right Shell) = outputStrLn "Already in shell-mode !"
interpretationResult c (Right r) = executeCommand c r
