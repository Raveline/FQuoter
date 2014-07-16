{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
module FQuoter.Serialize.Shortcuts
(
    DBError (..),
    insert
)
where

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Free

import FQuoter.Serialize.Serialize
import FQuoter.Serialize.SerializedTypes
import FQuoter.Parser.ParserTypes
import FQuoter.Serialize.Queries
import FQuoter.Quote

data DBError = NonExistingDataError String
             | AmbiguousDataError [String]

instance Show DBError where
    show (NonExistingDataError s) = s
    show (AmbiguousDataError s) = "Ambiguous input. Cannot choose between : " ++ unlines s

-- instance MonadError e m => MonadError e (Serialization m)

type FalliableSerialization m a = FreeT SerializationF (ExceptT DBError m) a

-- type FalliableSerialization' = (Monad m) => FreeT SerializationF (ExceptT DBError m)
-- type FalliableSerialization a = 
--    (MonadError DBError FalliableSerialization') => FalliableSerialization' a

conclude :: (Monad m) => FalliableSerialization m ()
conclude = commitAction >> return ()

insert :: (Monad m) => ParsedType -> FalliableSerialization m ()
{- Simply insert an author in the DB. -}
insert a@(PAuthor _) = create a
{- Insert a source in the DB. This is a three step
relation : first, insert the source in the Source table.
Then insert the link between Authors and the source.
Then, try to insert the metada. If the metadata are ambiguous,
an error is reported and the process is stopped.
NB: not fully implemented yet.
 -}
insert s@(PSource (ParserSource tit auth meta)) = 
    do create s
       idSource <- lastInsert 
       validatedAuthors <- mapM validateAuthor auth
       -- Do something with good authors
       mapM (insertMetadatas idSource) (Map.toList meta)
       return ()
       
validateAuthor :: (Monad m) => String -> FalliableSerialization m Integer
validateAuthor s = do
                    result <- search DBAuthor (ByName s)
                    case result of
                        [] -> throwError $ NonExistingDataError s
                        [dbv] -> return $ primaryKey dbv
                        res -> throwError $ AmbiguousDataError $ map (show . value) res

{- Insert metadatas. In the Metadata table, insert the value,
and the primary key to the related Metadata Type and Source. -}
insertMetadatas :: (Monad m) => PrimaryKey -> (String, String) -> FalliableSerialization m ()
insertMetadatas sourceId (info, value)
    = do pk <- readOrInsert DBMetadataInfo info
         associate2 (pk, sourceId) (PMetadataValue value)
         return ()

{- Currently deficient function - not generalized enough. -}
readOrInsert :: (Monad m) => DBType -> String -> FalliableSerialization m PrimaryKey
readOrInsert typ term = do
                        result <- search typ (ByName term)
                        case result of
                            []  -> do
                                create $ PMetadataInfo term
                                lastInsert
                            [dbv] -> return $ primaryKey dbv
                            _   -> error "Not handling multiple results."

