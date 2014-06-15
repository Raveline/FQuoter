module FQuoter.Serialize 
(
   DBAuthor
   ,serializeAuthor
   ,lookUpAuthor
   ,buildNewDB 
)
where

import Data.List.Split

import Database.HDBC
import Database.HDBC.Sqlite3

import FQuoter.Quote

type DBAuthor = DatabaseValue Author

insertAuthor = "INSERT INTO Author VALUES (?,?,?,?)"
findAuthor = "SELECT FROM Author\
            \WHERE first_name || \" \" || last_name like ?\
            \OR surname like ?"
updateAuthor = "UPDATE author SET first_name = ?, last_name = ?, surname = ? WHERE id_author = ?"
deleteAuthor = "DELETE FROM Author id_author = ?"

-- Save a non-existing author to the database
serializeAuthor :: (IConnection c) => c -> Author -> IO Integer
serializeAuthor conn = run conn insertAuthor . authorToSqlValues 

-- Look up an author by it's name or it's surname
lookUpAuthor :: (IConnection c) => c -> String -> IO [DBAuthor]
lookUpAuthor conn search = do
                        result <- quickQuery' conn insertAuthor 
                                    [toSql ("%" ++ search ++ "%")]
                        return $ map sqlValuesToAuthor result

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
sqlToMaybeString (SqlString []) = Nothing
sqlToMaybeString (SqlString s)  = Just s
sqlToMaybeString _              = Nothing

-- Create a new database from a schema.sql file
buildNewDB :: FilePath  -- DB to create
           -> FilePath  -- Schema file
           -> IO ()
buildNewDB toCreate schemaF = do
                                conn <- connectSqlite3 toCreate
                                commands <- readSchema schemaF
                                mapM (flip (run conn) []) . init $ commands
                                return ()

readSchema :: FilePath -> IO [String]
readSchema schemaF = do 
                        content <- readFile schemaF
                        return $ splitOn ";" content

