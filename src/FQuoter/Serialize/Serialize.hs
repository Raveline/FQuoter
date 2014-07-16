module FQuoter.Serialize.Serialize where

import Data.List.Split

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3

import FQuoter.Quote
import FQuoter.Parser.ParserTypes
import FQuoter.Serialize.SerializedTypes
import FQuoter.Serialize.Queries

--- UTITILITES
-- Make a Sqlvalue out of a Maybe String.
-- Nothing will be turned into an empty string
data SerializationF next
    = Create ParsedType next
    | Associate PairOfKeys next
    | Associate2 PairOfKeys ParsedType next
    | Search DBType SearchTerm ([DBValue SerializedType] -> next)
    | LastInsert (PrimaryKey -> next)
    | Update PrimaryKey SerializedType next
    | Delete SerializedType next
    | CommitAction next
    | RollbackAction next

instance Functor SerializationF where
    fmap f (Create st n) = Create st (f n)
    fmap f (Associate pks n) = Associate pks (f n)
    fmap f (Associate2 pks t n) = Associate2 pks t (f n)
    fmap f (Search typ term n) = Search typ term (f . n)
    fmap f (LastInsert n) = LastInsert (f . n)
    fmap f (CommitAction n) = CommitAction (f n)
    fmap f (RollbackAction n) = RollbackAction (f n)

type Serialization = FreeT SerializationF 

associate2 :: (Monad m) => PairOfKeys -> ParsedType -> Serialization m ()
associate2 pks t = liftF $ Associate2 pks t ()

create :: (Monad m) => ParsedType -> Serialization m ()
create t = liftF $ Create t ()

search :: (Monad m) => DBType -> SearchTerm -> Serialization m [DBValue SerializedType]
search typ term = liftF $ Search typ term id

lastInsert :: (Monad m) => Serialization m PrimaryKey
lastInsert = liftF $ LastInsert id

update :: (Monad m) => PrimaryKey -> SerializedType -> Serialization m ()
update pk st = liftF $ Update pk st ()

commitAction :: (Monad m) => Serialization m ()
commitAction = liftF $ CommitAction ()

rollbackAction :: (Monad m) => Serialization m ()
rollbackAction = liftF $ RollbackAction ()

process :: (MonadIO m, IConnection c) => Serialization m next -> c -> m next
process fr c = do
        v <- runFreeT fr
        case  v of
            Pure r -> return r
            Free (Create t n) -> do liftIO $ c <~ t 
                                    >> process n c
            Free (Search t s n) -> do liftIO $ c ~> (t,s) 
                                      >>= mapM (return . unsqlizeST t) 
                                      >>= flip process c . n
            Free (Associate2 pks t n) -> do liftIO $ c <~~ (pks, t) 
                                            >> process n c
            Free (LastInsert n) -> do liftIO $ queryLastInsert c 
                                      >>= flip process c . n
            Free (CommitAction n) -> do liftIO $ commit c 
                                        >> process n c
            Free (RollbackAction n) -> do liftIO $ rollback c 
                                          >> process n c

(<~) :: (IConnection c) => c -> ParsedType -> IO ()
conn <~ s = void (run conn (getInsert s) (sqlize s) )

(~>) :: (IConnection c) => c -> (DBType, SearchTerm) -> IO [[SqlValue]]
conn ~> (t,st) = uncurry (lookUp conn) (t,st)

(<~~) :: (IConnection c) => c -> (PairOfKeys, ParsedType) -> IO  ()
conn <~~ (p,  t) = void(run conn (getInsert t) (SqlNull:sqlizePair p))

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

