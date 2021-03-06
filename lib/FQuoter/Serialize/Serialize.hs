module FQuoter.Serialize.Serialize 
( 
  SerializationF (..)
 ,SerializationT
 ,Serialization
 ,create
 ,associate
 ,associate2
 ,dissociate
 ,search
 ,lastInsert
 ,update
 ,delete
 ,commitAction
 ,rollbackAction
 ,process
 ,readSchema
 ,buildNewDB
)
where

import Data.List hiding (delete)
import Data.List.Split

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Reader
import Database.HDBC
import Database.HDBC.Sqlite3

import FQuoter.Parser.ParserTypes
import FQuoter.Serialize.SerializedTypes
import FQuoter.Serialize.Grouping
import FQuoter.Serialize.Queries

data SerializationF next
    = Create ParsedType next
    | Associate PairOfTypes PairOfKeys next
    | Associate2 PairOfKeys ParsedType next
    | Dissociate PairOfTypes (Either PrimaryKey PairOfKeys) next
    | Search DBType SearchTerm ([DBValue SerializedType] -> next)
    | LastInsert (PrimaryKey -> next)
    | Update PrimaryKey ParsedType next
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
    fmap f (Update pk st n) = Update pk st (f n)
    fmap f (Dissociate pts pks n) = Dissociate pts pks (f n)

type Serialization = FreeF SerializationF
type SerializationT = FreeT SerializationF

associate :: (Monad m) => PairOfTypes -> PairOfKeys -> SerializationT m ()
associate pts pks = liftF $ Associate pts pks ()

dissociate :: (Monad m) => PairOfTypes -> Either PrimaryKey PairOfKeys -> SerializationT m ()
dissociate pts pks = liftF $ Dissociate pts pks ()

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

update :: (Monad m) => PrimaryKey -> ParsedType -> SerializationT m ()
update pk pt = liftF $ Update pk pt ()

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
process' (Free (Dissociate pot pok n)) = do c <- ask
                                            liftIO $ c <~/> (pot, pok)
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

process' (Free (Update pk o n)) = do c <- ask
                                     liftIO $ c <~* (o, pk)
                                     process n

{- Insertion -}
(<~) :: (IConnection c) => c -> ParsedType -> IO ()
conn <~ s = void (run conn (getInsert s) (SqlNull:sqlize s) )

(</~) :: (IConnection c) => c -> (DBType, PrimaryKey) -> IO ()
conn </~ (t,k) = void $ run conn (getDelete t) [toSql k]

{- Search -}
(~>) :: (IConnection c) => c -> (DBType, SearchTerm) -> IO [[SqlValue]]
conn ~> (t,st) = uncurry (lookUp conn) (t,st)

{- Associate data in a many-to-many relation -}
(<~>) :: (IConnection c) => c -> (PairOfTypes, PairOfKeys) -> IO ()
conn <~> (ts, ks) = void(run conn query (SqlNull:sqlizePair ks))
    where query = uncurry getAssociate ts

(<~/>) :: (IConnection c) => c -> (PairOfTypes, Either PrimaryKey PairOfKeys) -> IO ()
conn <~/> (ts, Left pk) = void(run conn query [toSql pk])
    where query= uncurry getFullDissociate ts
conn <~/> (ts, Right pks) = void(run conn query $ sqlizePair pks)
    where query = uncurry getDissociate ts

{- Associate data in a triple relationship -}
(<~~) :: (IConnection c) => c -> (PairOfKeys, ParsedType) -> IO  ()
conn <~~ (p,  t@(PMetadataValue s)) = void(run conn (getInsert t) (SqlNull:toSql s:sqlizePair p))
_ <~~ (_, _) = error "Unsupported type for Association2."

{- Update -}
(<~*) :: (IConnection c) => c -> (ParsedType, PrimaryKey) -> IO ()
conn <~* (st, pk) = void (run conn query (sqlize st ++ [toSql pk]))
    where
        query = getUpdate st

sqlizePair :: PairOfKeys -> [SqlValue]
sqlizePair (k1,k2) = [toSql k1, toSql k2]

--- IO methods
lookUp :: (IConnection c) => c 
                                                -> DBType 
                                                -> SearchTerm 
                                                -> IO [[SqlValue]]
lookUp conn t st = quickQuery' conn sQuery (searchArray sQuery st)
    where sQuery = getSearch t st

-- Get the id of last insertion
queryLastInsert :: (IConnection c) => c -> IO Integer
queryLastInsert conn = do
                    result <- quickQuery' conn "select last_insert_rowid()" []
                    return $ fromSql . head . head $ result

-- For a given search, put as many "%<search_term>%" as needed by
-- the query
searchArray :: String -> SearchTerm -> [SqlValue]
searchArray query (ByName s) = replicate (paramNumber query) 
                                $ toSql $ "%" ++ s ++ "%"
    where 
        paramNumber = length . filter ('?' ==)
searchArray _ (ById i) = [toSql i]
searchArray _ (ByIn xs) = [toSql . intercalate "," $ xs]
searchArray _ (ByAssociation _ i) = [toSql i]

-- Convert an author to a list of SqlValue for insertion
-- Create a new database from a schema.sql file
buildNewDB :: FilePath  -- Schema file
           -> FilePath  -- DB to create
           -> IO ()
buildNewDB schemaF toCreate = do
                                conn <- connectSqlite3 toCreate
                                commands <- readSchema schemaF
                                mapM_ (flip (run conn) []) . init $ commands
                                commit conn
                                disconnect conn
                                return ()

readSchema :: FilePath -> IO [String]
readSchema schemaF = do 
                        content <- readFile schemaF
                        return $ splitOn ";" content

