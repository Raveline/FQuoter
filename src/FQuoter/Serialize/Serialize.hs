module FQuoter.Serialize.Serialize
(
    Serialization
    ,process
    ,buildNewDB
    ,insertAuthor
)
where

import Data.List.Split

import Control.Monad.Free
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Data.Map as Map

import FQuoter.Quote
import FQuoter.Serialize.SerializedTypes
import FQuoter.Serialize.Queries


--- UTITILITES
-- Make a Sqlvalue out of a Maybe String.
-- Nothing will be turned into an empty string
data SerializationF next
    = Create SerializedType next
    | Associate PairOfKeys next
    | Associate2 PairOfKeys SerializedType next
    | Search DBType SearchTerm ([DBValue SerializedType] -> next)
    | LastInsert (PrimaryKey -> next)
    | Update PrimaryKey SerializedType next
    | Delete SerializedType next

instance Functor SerializationF where
    fmap f (Create st n) = Create st (f n)
    fmap f (Associate pks n) = Associate pks (f n)
    fmap f (Associate2 pks t n) = Associate2 pks t (f n)
    fmap f (Search typ term n) = Search typ term (f . n)
    fmap f (LastInsert n) = LastInsert (f . n)

type Serialization = Free (SerializationF)

associate2 :: PairOfKeys -> SerializedType -> Serialization ()
associate2 pks t = liftF $ Associate2 pks t ()

create :: SerializedType -> Serialization ()
create t = liftF $ Create t ()

search :: DBType -> SearchTerm -> Serialization ([DBValue SerializedType])
search typ term = liftF $ Search typ term $ id

lastInsert :: Serialization PrimaryKey
lastInsert = liftF $ LastInsert $ id

update :: PrimaryKey -> SerializedType -> Serialization ()
update pk st = liftF $ Update pk st ()

process :: (IConnection c) => c -> Serialization next -> IO next
process _ (Pure r) = return r
process conn (Free (Create t n)) = conn <~ t >> process conn n
process conn (Free (Search t s n)) = conn ~> (t,s) 
                                >>= mapM (return . unsqlizeST t) 
                                >>= process conn . n
process conn (Free (Associate2 pks t n)) = conn <~~ (pks, t) >> process conn n

(<~) :: (IConnection c) => c -> SerializedType -> IO ()
conn <~ s = runInsert conn (getInsert s) (sqlizeST s) >> return ()

(~>) :: (IConnection c) => c -> (DBType, SearchTerm) -> IO [[SqlValue]]
conn ~> (t,st) = uncurry (lookUp conn) (t,st)

(<~~) :: (IConnection c) => c -> (PairOfKeys, SerializedType) -> IO ()
conn <~~ (p,  t) = runInsert conn (getInsert t) (SqlNull:(sqlizePair p))
                    >> return ()

sqlizePair :: PairOfKeys -> [SqlValue]
sqlizePair (k1,k2) = [toSql k1, toSql k2]

--- IO methods
lookUp :: (IConnection c) => c 
                                                -> DBType 
                                                -> SearchTerm 
                                                -> IO [[SqlValue]]
lookUp conn t st = quickQuery' conn squery (searchArray squery st)
    where squery = getSearch t st

runInsert :: (IConnection c) => c -> Query -> [SqlValue] -> IO Integer
runInsert conn query vs = run conn query vs

runAssociate :: (IConnection c) => c -> Query -> [Integer] -> IO Integer
runAssociate conn query = run conn query . ((:)SqlNull) . map toSql

-- Get the id of last insertion
queryLastInsert :: (IConnection c) => c -> IO Integer
queryLastInsert conn = do
                    result <- quickQuery' conn "last_insert_rowid()" $ []
                    return $ fromSql . head . head $ result


-- Insertion mechanisms

insertAuthor :: Author -> Serialization ()
insertAuthor = create . SAuthor

insertSource :: Source -> Serialization ()
insertSource s = 
    do
        create $ SSource s
        idSource <- lastInsert 
        return $ Map.mapWithKey (insertMetadatas idSource) (metadata s)
        return ()

insertMetadatas :: PrimaryKey -> MetadataInfo -> MetadataValue -> Serialization ()
insertMetadatas sourceId info value 
    = do pk <- readOrInsert DBMetadataInfo (metadataInfo info)
         associate2 (pk, sourceId) (SMetadataValue value)
         return ()
                         
readOrInsert :: DBType -> String -> Serialization PrimaryKey
readOrInsert typ term = do
                        result <- search typ (ByName term)
                        case result of
                            []  -> do
                                create $ SMetadataInfo (MetadataInfo $ QuoterString term)
                                lastInsert
                            [dbv] -> return $ primary_key dbv
                            _   -> error "Not handling multiple results."

-- For a given search, put as many "%<search_term>%" as needed by
-- the query
searchArray :: String -> SearchTerm -> [SqlValue]
searchArray query (ByName search) = replicate (paramNumber query) $ toSql $ "%" ++ search ++ "%"
    where 
        paramNumber = length . filter ((==) '?')
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
