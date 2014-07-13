import System.Environment

import FQuoter.Parser.ParserTypes
import FQuoter.Parser.Parser
import FQuoter.Serialize.Serialize
import FQuoter.Serialize.Shortcuts
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad.Error
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

checkNonExisting :: DBType -> [String] -> [SerializedType]
checkNonExisting typ = undefined -- map (doesExist typ)

insertAndDisplay :: ParsedType -> IO ()
insertAndDisplay a = do db <- getDB 
                        {- Our current issue.
                        process is : 
                            Serialization n -> Connection -> IO ()
                        our (insert a) is :
                            FaillableSerialization ()
                        (Issue 1) So we need a way to make
                            (FaillableSerialization a -> Serialization a)
                        Furthemore, We know that runErrorT will give us a 
                        Either DBError (IO ()), only if it has an ErrorT e a.
                        Which is the type of our FalliableSerialization.
                        (2) So our process need to produce a FaillableSerialization
                        We know that lift SHOULD meet this second contiion.
                        No idea how to meet the first one. -}
                        result <- runErrorT $ return $ lift process $ insert a
                        case result of
                            Right _ -> return ()
                            Left e -> putStrLn . show $ e

handleError :: DBError -> IO ()
handleError = putStrLn . show

-- Obviously temporary
getDB :: IO Connection
getDB = connectSqlite3 "test.db"
