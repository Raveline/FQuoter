import System.Environment
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad.Except

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
executeCommand _ = putStrLn "Not implemented yet."

insertAndDisplay :: ParsedType -> IO ()
insertAndDisplay a = do db <- getDB 
                        result <- runExceptT $ process (insert a) db
                        case result of
                            Right _ -> do 
                                        putStrLn $ "Inserted " ++ (show a)
                                        return ()
                            Left e -> displayException e

displayException :: DBError -> IO ()
displayException (NonExistingDataError s) = putStrLn $ "The following author does not exist : " ++ s
displayException (AmbiguousDataError s) = putStrLn $ "Too many potential authors with your input. There are the possible choices : " ++ (unlines s)

handleError :: DBError -> IO ()
handleError = putStrLn . show

-- Obviously temporary
getDB :: IO Connection
getDB = connectSqlite3 "test.db"
