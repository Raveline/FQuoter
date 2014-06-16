module FQuoter.SerializeSpec(main, spec) where

import Control.Monad

import System.Directory
import Test.Hspec
import FQuoter.Serialize
import FQuoter.Quote
import Database.HDBC.Sqlite3

henryJames = Author (Just "Henry") (Just "James") Nothing

dbName = "test.db"
schema = "schema.sql"

main :: IO()
main = hspec spec

serializeAndDeserialize :: Author       -- An author to serialize
                        -> String       -- The lookup string to use
                        -> IO Author
serializeAndDeserialize a lookup = do
                                conn <- connectSqlite3 dbName
                                serializeAuthor conn a
                                result <- lookUpAuthor conn lookup
                                return $ value $ head result

spec = before (buildNewDB schema dbName) $ do
    describe "Check database creation." $ do 
        it "Checks DB generation works" $ do
            doesFileExist dbName `shouldReturn` True
        it "Serialize an Author, gets it, and check it's the same" $ do
            serializeAndDeserialize henryJames "Henry" `shouldReturn` henryJames
