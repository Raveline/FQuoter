module FQuoter.Serialize.Serialize where

import Data.List.Split

import Control.Monad
import Control.Monad.Free
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3

import FQuoter.Quote
import FQuoter.Parser.ParserTypes
import FQuoter.Serialize.SerializedTypes
import FQuoter.Serialize.Queries

type SuccessMessage = String
type DBActionResult = Either DBError SuccessMessage

data DBError
    = UnexistingData String
    | AmbiguousData String [String]
    deriving(Eq, Show)

--- UTITILITES
-- Make a Sqlvalue out of a Maybe String.
-- Nothing will be turned into an empty string
data SerializationF next
    = Create ParsedType (DBActionResult -> next)
    | Associate PairOfKeys next
    | Associate2 PairOfKeys ParsedType next
    | Search DBType SearchTerm ([DBValue SerializedType] -> next)
    | LastInsert (PrimaryKey -> next)
    | Update PrimaryKey SerializedType next
    | Delete SerializedType next

instance Functor SerializationF where
    fmap f (Create st n) = Create st (f . n)
    fmap f (Associate pks n) = Associate pks (f n)
    fmap f (Associate2 pks t n) = Associate2 pks t (f n)
    fmap f (Search typ term n) = Search typ term (f . n)
    fmap f (LastInsert n) = LastInsert (f . n)

type Serialization = Free SerializationF

associate2 :: PairOfKeys -> ParsedType -> Serialization ()
associate2 pks t = liftF $ Associate2 pks t ()

create :: ParsedType -> Serialization DBActionResult
create t = liftF $ Create t id

search :: DBType -> SearchTerm -> Serialization [DBValue SerializedType]
search typ term = liftF $ Search typ term id

lastInsert :: Serialization PrimaryKey
lastInsert = liftF $ LastInsert id

update :: PrimaryKey -> SerializedType -> Serialization ()
update pk st = liftF $ Update pk st ()

process :: (IConnection c) => c -> Serialization next -> IO next
process _ (Pure r) = return r
process conn (Free (Create t n)) = conn <~ t >>= process conn . n
process conn (Free (Search t s n)) = conn ~> (t,s) 
                                >>= mapM (return . unsqlizeST t) 
                                >>= process conn . n
process conn (Free (Associate2 pks t n)) = conn <~~ (pks, t) >> process conn n
process conn (Free (LastInsert n)) = queryLastInsert conn  >>= process conn . n

(<~) :: (IConnection c) => c -> ParsedType -> IO DBActionResult
conn <~ s = run conn (getInsert s) (sqlize s) >> ok s

(~>) :: (IConnection c) => c -> (DBType, SearchTerm) -> IO [[SqlValue]]
conn ~> (t,st) = uncurry (lookUp conn) (t,st)

(<~~) :: (IConnection c) => c -> (PairOfKeys, ParsedType) -> IO  DBActionResult
conn <~~ (p,  t) = run conn (getInsert t) (SqlNull:sqlizePair p) >> ok t

ok :: ParsedType -> IO DBActionResult
ok s = return $ Right $ "Inserted : " ++ (show s)

sqlizePair :: PairOfKeys -> [SqlValue]
sqlizePair (k1,k2) = [toSql k1, toSql k2]

--- IO methods
lookUp :: (IConnection c) => c 
                                                -> DBType 
                                                -> SearchTerm 
                                                -> IO [[SqlValue]]
lookUp conn t st = quickQuery' conn squery (searchArray squery st)
    where squery = getSearch t st

runAssociate :: (IConnection c) => c -> Query -> [Integer] -> IO Integer
runAssociate conn query = run conn query . (:) SqlNull . map toSql

-- Get the id of last insertion
queryLastInsert :: (IConnection c) => c -> IO Integer
queryLastInsert conn = do
                    result <- quickQuery' conn "select last_insert_rowid()" []
                    return $ fromSql . head . head $ result

-- For a given search, put as many "%<search_term>%" as needed by
-- the query
searchArray :: String -> SearchTerm -> [SqlValue]
searchArray query (ByName search) = replicate (paramNumber query) $ toSql $ "%" ++ search ++ "%"
    where 
        paramNumber = length . filter ('?' ==)
searchArray query (ById id) = [toSql id]

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

