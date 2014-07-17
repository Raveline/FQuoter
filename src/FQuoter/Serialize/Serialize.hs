module FQuoter.Serialize.Serialize where

import Data.List.Split

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Reader
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

process :: (IConnection c, MonadIO m) => Serialization (ReaderT c m) next -> ReaderT c m next
process fr = do
        v <- runFreeT fr
        case  v of
            Pure r -> return r
            Free (Create t n) -> do c <- ask
                                    liftIO $ c <~ t 
                                    process n 
            Free (Search t s n) -> do c <- ask
                                      v <- liftIO $ c ~> (t,s) 
                                      v' <- mapM (return . unsqlizeST t) v
                                      process (n v')
            Free (Associate2 pks t n) -> do c <- ask
                                            liftIO $ c <~~ (pks, t)
                                            process n
            Free (LastInsert n) -> do c <- ask
                                      l <- liftIO $ queryLastInsert c 
                                      process (n l)
            Free (CommitAction n) -> do c <- ask
                                        liftIO $ commit c 
                                        process n
            Free (RollbackAction n) -> do c <- ask
                                          liftIO $ rollback c 
                                          process n

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

