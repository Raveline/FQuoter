{-# LANGUAGE FlexibleContexts #-}
module FQuoter.Serialize 
(
   buildNewDB 
)
where

import Data.List.Split

import Control.Monad.Free
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Data.Map as Map

import FQuoter.Quote

-- Queries constants
qInsertAuthor = "INSERT INTO Author VALUES (?,?,?,?)"
qInsertSource = "INSERT INTO Source VALUES (?, ?)"
qInsertMetadata = "INSERT INTO MetadataInfo VALUES (?, ?)"
qAssociateMetadata = "INSERT INTO MetadataValue VALUES (?,?,?)"

qFindAuthor = "SELECT * FROM Author \
            \WHERE first_name || \" \" || last_name like ? \
            \OR surname like ?"
qReadAuthor = "SELECT * FROM Author WHERE id_author = ?"
qFindMetadata = "SELECT * FROM MetadataInfo WHERE  name like ?"

qUpdateAuthor = "UPDATE author SET first_name = ?, last_name = ?, surname = ? WHERE id_author = ?"

qDeleteAuthor = "DELETE FROM Author id_author = ?"
--- UTITILITES

data SerializedType 
    = SAuthor Author
    | SSource Source
    | SQuote Quote
    | SMetadataInfo MetadataInfo
    | SMetadataDictionary MetadataDictionary

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

---- TYPES

type Query = String
-- A simple container to hold a data type and
-- its primary key in the database.
data DBValue a = DBValue { primary_key :: Integer
                                     , value :: a
                         }

-- Anything that can be serialized must be translated in a list of
-- SqlValues, and can be translated back from it. We will however
-- stored those "translated" objects as DBValues, wich will contain
-- the primary key.
class (Show a) => SqliteSerializable a where
    sqlize :: a -> [SqlValue]
    unsqlize :: [SqlValue] -> DBValue a
    searchTerm :: a -> String
    searchTerm = show

instance SqliteSerializable Author where
    sqlize (Author fName lName sName) = [SqlNull
                                        ,maybeStringToSql fName
                                        ,maybeStringToSql lName
                                        ,maybeStringToSql sName]
    unsqlize (pkey:fName:lName:sName:[]) = DBValue (fromSql pkey) toAuthor
        where
            toAuthor = Author (sqlToMaybeString fName)
                                 (sqlToMaybeString lName)
                                 (sqlToMaybeString sName)

instance SqliteSerializable Source where
    sqlize (Source title _ _) = [SqlNull
                                ,toSql title]
    unsqlize (pkey:title:[]) =  DBValue (fromSql pkey) 
                                (Source (fromSql title) [] Map.empty)

instance SqliteSerializable QuoterString where
    sqlize s = [SqlNull, toSql (string s)]
    unsqlize (pkey:s:[]) = DBValue (fromSql pkey) (QuoterString(fromSql s))

data SearchTerm 
    = ById Integer
    | ByName String

data SerializationF next
    = Create SerializedType next
    | Search SearchTerm (SerializedType -> next)
    | LastInsert (Integer -> next)
    | Update Integer SerializedType next
    | Delete SerializedType next

instance Functor (SerializationF a) where
    fmap f (Create t q item n) = Create q item (f n)
    fmap f (Search term n) = Search term (f . n)
    fmap f (LastInsert n) = LastInsert (f . n)

type Serialization a = Free (SerializationF a)

create :: SerializedType -> Serialization ()
create = liftF $ Create

search :: String -> Serialization SerializedType
search s = liftF $ Search s $ id

-- update :: (SqliteSerializable a) => Query -> a -> Serialization ()
-- update = liftF $ Update

-- delete :: (SqliteSerializable a) => Query -> a -> Serialization ()
-- delete = liftF $ Delete

process :: (IConnection c, SqliteSerializable a) => c -> Serialization a r -> IO r
process _ (Pure r) = return r
process conn (Free (Create (SAuthor a) n)) = runInsert conn qInsertAuthor (sqlize a) >> process conn n
process conn (Free (Create (SSource s) n)) = runInsert conn qInsertSource (sqlize s) >> process conn n
process conn (Free (Create (SMetadataInfo s) n)) = runInsert conn qInsertMetadata (sqlize s) >> process conn n
-- process conn (Free (Lookup s)) = runLookup conn q (searchArray q l) >>= process conn . n
-- process conn (Free (LastInsert n)) = queryLastInsert conn >>= process conn . n
-- process conn (Free (Associate q ids n)) = runAssociate conn q ids >> process conn n

--- IO methods
runInsert :: (IConnection c, SqliteSerializable a) => c -> Query -> a -> IO Integer
runInsert conn query = run conn query . sqlize

runAssociate :: (IConnection c) => c -> Query -> [Integer] -> IO Integer
runAssociate conn query = run conn query . ((:)SqlNull) . map toSql

runLookup :: (IConnection c
             , SqliteSerializable a) => c
                                     -> Query
                                     -> [SqlValue]
                                     -> IO [DBValue a]
runLookup conn query search = 
        do
            results <- quickQuery' conn query search
            return $ map unsqlize results

-- Get the id of last insertion
queryLastInsert :: (IConnection c) => c -> IO Integer
queryLastInsert conn = do
                    result <- quickQuery' conn "last_insert_rowid()" $ []
                    return $ fromSql . head . head $ result


-- Insertion mechanisms
insertAuthor :: Author -> Serialization Author ()
insertAuthor = create

insertSource :: Source -> Serialization Source ()
insertSource s = 
    do
        create s
        idSource <- lastInsert 
        -- let dict = (insertMetadatas $ metadata s)
        -- insertMetadatasDict idSource dict
        return ()

-- insertMetadatasDict :: Integer -> MetadataDictionary -> Serialization MetadataDictionary ()
insertMetadatasDict id dict = Map.mapWithKey associate dict
    where
        asssociate k v = associate qAssociateMetadata [k, id]

insertMetadatas metadatas = do 
                        let keys = map (readOrInsert qFindMetadata qInsertMetadata) $ Map.keys metadatas
                        return $ Map.fromList (zip keys (Map.elems $ metadatas))

readOrInsert :: (SqliteSerializable a) => Query  -- For reading
                                       -> Query  -- For inserting
                                       -> a      -- Object
                                       -> Serialization a Integer
readOrInsert qR qI a = do
                        result <- lookUp qR (searchTerm a)
                        case result of
                            []  -> do
                                insert qI a
                                lastInsert
                            [dbv] -> return $ primary_key dbv
                            _   -> error "Not handling multiple results."


-- For a given search, put as many "%<search_term>%" as needed by
-- the query
searchArray :: String -> String -> [SqlValue]
searchArray query search = replicate (paramNumber query) $ toSql $ "%" ++ search ++ "%"
    where 
        paramNumber = length . filter ((==) '?')

-- Convert an author to a list of SqlValue for insertion
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
