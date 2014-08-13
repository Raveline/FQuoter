module FQuoter.Serialize.Serialize where

import Data.List
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
import FQuoter.Serialize.Grouping
import FQuoter.Serialize.Queries

--- UTITILITES
-- Make a Sqlvalue out of a Maybe String.
-- Nothing will be turned into an empty string
data SerializationF next
    = Create ParsedType next
    | Associate PairOfTypes PairOfKeys next
    | Associate2 PairOfKeys ParsedType next
    | Search DBType SearchTerm ([DBValue SerializedType] -> next)
    | LastInsert (PrimaryKey -> next)
    | Update PrimaryKey SerializedType next
    | Delete DBType PrimaryKey next
    | CommitAction next
    | RollbackAction next

instance Functor SerializationF where
    fmap f (Create st n) = Create st (f n)
    fmap f (Associate pts pks n) = Associate pts pks (f n)
    fmap f (Associate2 pks t n) = Associate2 pks t (f n)
    fmap f (Search typ term n) = Search typ term (f . n)
    fmap f (LastInsert n) = LastInsert (f . n)
    fmap f (CommitAction n) = CommitAction (f n)
    fmap f (RollbackAction n) = RollbackAction (f n)
    fmap f (Delete t pk n) = Delete t pk (f n)

type Serialization = FreeF SerializationF
type SerializationT = FreeT SerializationF

associate :: (Monad m) => PairOfTypes -> PairOfKeys -> SerializationT m ()
associate pts pks = liftF $ Associate pts pks ()

associate2 :: (Monad m) => PairOfKeys -> ParsedType -> SerializationT m ()
associate2 pks t = liftF $ Associate2 pks t ()

create :: (Monad m) => ParsedType -> SerializationT m ()
create t = liftF $ Create t ()

delete :: (Monad m) => DBType -> PrimaryKey -> SerializationT m ()
delete t k = liftF $ Delete t k ()

search :: (Monad m) => DBType -> SearchTerm -> SerializationT m [DBValue SerializedType]
search typ term = liftF $ Search typ term id

lastInsert :: (Monad m) => SerializationT m PrimaryKey
lastInsert = liftF $ LastInsert id

update :: (Monad m) => PrimaryKey -> SerializedType -> SerializationT m ()
update pk st = liftF $ Update pk st ()

commitAction :: (Monad m) => SerializationT m ()
commitAction = liftF $ CommitAction ()

rollbackAction :: (Monad m) => SerializationT m ()
rollbackAction = liftF $ RollbackAction ()

process :: (IConnection c, MonadIO m) => SerializationT (ReaderT c m) next -> ReaderT c m next
process fr = runFreeT fr >>= process'

process'  :: (IConnection c, MonadIO m) => Serialization next (SerializationT (ReaderT c m) next) -> ReaderT c m next
process' (Pure r) = return r
process' (Free (Create t n)) = do c <- ask
                                  liftIO $ c <~ t 
                                  process n 
process' (Free (Search t s n)) = do c <- ask
                                    v <- liftIO $ c ~> (t,s) 
                                    v' <- mapM (return . unsqlizeST t) (groupSql t v)
                                    process (n v')
process' (Free (Associate pot pok n)) = do c <- ask
                                           liftIO $ c <~> (pot, pok)
                                           process n
process' (Free (Associate2 pks t n)) = do c <- ask
                                          liftIO $ c <~~ (pks, t)
                                          process n
process' (Free (LastInsert n)) = do c <- ask
                                    l <- liftIO $ queryLastInsert c 
                                    process (n l)
process' (Free (CommitAction n)) = do c <- ask
                                      liftIO $ commit c 
                                      process n
process' (Free (Delete t k n)) = do c <- ask
                                    liftIO $ c </~ (t, k)
                                    process n
process' (Free (RollbackAction n)) = do c <- ask
                                        liftIO $ rollback c 
                                        process n

{- Insertion -}
(<~) :: (IConnection c) => c -> ParsedType -> IO ()
conn <~ s = void (run conn (getInsert s) (sqlize s) )

(</~) :: (IConnection c) => c -> (DBType, PrimaryKey) -> IO ()
conn </~ (t,k) = void $ run conn (getDelete t) [toSql k]

{- Search -}
(~>) :: (IConnection c) => c -> (DBType, SearchTerm) -> IO [[SqlValue]]
conn ~> (t,st) = uncurry (lookUp conn) (t,st)

{- Associate data in a many-to-many relation -}
(<~>) :: (IConnection c) => c -> (PairOfTypes, PairOfKeys) -> IO ()
conn <~> (ts, ks) = void(run conn query (SqlNull:sqlizePair ks))
        where
            query = uncurry getAssociate ts
{- Associate data in a triple relationship -}
(<~~) :: (IConnection c) => c -> (PairOfKeys, ParsedType) -> IO  ()
conn <~~ (p,  t@(PMetadataValue s)) = void(run conn (getInsert t) (SqlNull:toSql s:sqlizePair p))
conn <~~ (p, _) = error "Unsupported type for Association2."

sqlizePair :: PairOfKeys -> [SqlValue]
sqlizePair (k1,k2) = [toSql k1, toSql k2]

--- IO methods
lookUp :: (IConnection c) => c 
                                                -> DBType 
                                                -> SearchTerm 
                                                -> IO [[SqlValue]]
lookUp conn t term@(ByAssociation typ id) = quickQuery' conn (getSearch t term) [toSql id]
lookUp conn t term@(ByIn xs) = quickQuery' conn (getSearch t term) [toSql . intercalate "," $ xs]
lookUp conn t st = quickQuery' conn sQuery (searchArray sQuery st)
    where sQuery = getSearch t st

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

