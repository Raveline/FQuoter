module FQuoter.Serialize 
(
   DBAuthor
   ,serializeAuthor
   ,lookUpAuthor
   ,buildNewDB 
)
where

import Data.List.Split

import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3

import FQuoter.Quote

type DBAuthor = DatabaseValue Author

insertAuthor = "INSERT INTO Author VALUES (?,?,?,?)"
findAuthor = "SELECT * FROM Author \
            \WHERE first_name || \" \" || last_name like ? \
            \OR surname like ?"
updateAuthor = "UPDATE author SET first_name = ?, last_name = ?, surname = ? WHERE id_author = ?"
deleteAuthor = "DELETE FROM Author id_author = ?"

-- Save a non-existing author to the database
serializeAuthor :: (IConnection c) => c -> Author -> IO Integer
serializeAuthor conn = run conn insertAuthor . authorToSqlValues 

-- Look up an author by it's name or it's surname
lookUpAuthor :: (IConnection c) => c -> String -> IO [DBAuthor]
lookUpAuthor conn search = do
                        result <- quickQuery' conn findAuthor
                            $ searchArray search
                        return $ map sqlValuesToAuthor result

searchArray :: String -> [SqlValue]
searchArray search = replicate 2 $ toSql $ "%" ++ search ++ "%"

-- Convert an author to a list of SqlValue for insertion
authorToSqlValues :: Author -> [SqlValue]
authorToSqlValues (Author fName lName sName) = [SqlNull
                                               ,maybeStringToSql fName
                                               ,maybeStringToSql lName
                                               ,maybeStringToSql sName]

sqlValuesToAuthor :: [SqlValue] -> DBAuthor
sqlValuesToAuthor (pkey:fName:lName:sName:[]) = 
    DatabaseValue (fromSql pkey) sqlValuesToAuthor'
    where
        sqlValuesToAuthor' = Author (sqlToMaybeString fName)
                             (sqlToMaybeString lName)
                             (sqlToMaybeString sName)

-- Make a Sqlvalue out of a Maybe String.
-- Nothing will be turned into an empty string
maybeStringToSql :: Maybe String -> SqlValue
maybeStringToSql Nothing        = toSql ""
maybeStringToSql (Just s)       = toSql s

sqlToMaybeString :: SqlValue -> Maybe String
sqlToMaybeString s@(SqlByteString _)  = stringToMaybe $ fromSql s 
    where
        stringToMaybe :: String -> Maybe String
        stringToMaybe [] = Nothing
        stringToMaybe s = Just s
sqlToMaybeString _ = Nothing

-- Create a new database from a schema.sql file
buildNewDB :: FilePath  -- Schema file
           -> FilePath  -- DB to create
           -> IO ()
buildNewDB schemaF toCreate = do
                                conn <- connectSqlite3 toCreate
                                commands <- readSchema schemaF
                                mapM (flip (run conn) []) . init $ commands
                                commit conn
                                disconnect conn
                                return ()

readSchema :: FilePath -> IO [String]
readSchema schemaF = do 
                        content <- readFile schemaF
                        return $ splitOn ";" content

