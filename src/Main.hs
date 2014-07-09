import System.Environment

import FQuoter.Parser
import FQuoter.Serialize.Serialize
import Database.HDBC.Sqlite3

main :: IO ()
main = do
        args <- getArgs 
        case parseInput(unwords args) of 
            Left s -> putStrLn (show s)
            Right c -> executeCommand c

executeCommand :: Action -> IO ()
executeCommand (Insert (TAuthor author)) = insertAndDisplay insertAuthor author
executeCommand _ = putStrLn "Not implemented yet."

insertAndDisplay :: (Show a) => (a -> Serialization ()) -> a -> IO ()
insertAndDisplay f a = do
                        db <- getDB 
                        process db $ f a
                        putStrLn $ "Added : " ++ show a

-- Obviously temporary
getDB :: IO Connection
getDB = connectSqlite3 "test.db"
