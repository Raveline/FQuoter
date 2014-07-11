import System.Environment

import FQuoter.Parser.ParserTypes
import FQuoter.Parser.Parser
import FQuoter.Serialize.Shortcuts
import Database.HDBC.Sqlite3
import FQuoter.Serialize.SerializedTypes
import FQuoter.Serialize.Serialize

main :: IO ()
main = do
        args <- getArgs 
        case parseInput(unwords args) of 
            Left s -> putStrLn (show s)
            Right c -> executeCommand c

executeCommand :: Action -> IO ()
executeCommand (Insert x) = insertAndDisplay x
executeCommand _ = putStrLn "Not implemented yet."

checkNonExisting :: DBType -> [String] -> [SerializedType]
checkNonExisting typ = undefined -- map (doesExist typ)

insertAndDisplay :: ParsedType -> IO ()
insertAndDisplay a = do
                        putStrLn $ "Adding... " ++ show a
                        db <- getDB 
                        process db $ insert a
                        putStrLn $ "Added : " ++ show a

-- Obviously temporary
getDB :: IO Connection
getDB = connectSqlite3 "test.db"
