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
import Control.Monad.Reader

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


type FalliableSerialization r m a = FreeT SerializationF (ReaderT r (ExceptT DBError m)) a

insert :: (Monad m) => ParsedType -> FalliableSerialization r m ()
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
       mapM (associate (DBSource, DBAuthor) . (,) idSource) validatedAuthors
       mapM (insertMetadatas idSource) (Map.toList meta)
       return ()
insert q@(PQuote pq@(ParserQuote content source loc tags comments)) =
    do source <- getPrimaryKey DBSource source
       let q' = PLinkedQuote $ LinkedQuote pq source
       create q'
       idQuote <- lastInsert
       -- associate (DBQuote, DBAuthor) (idQuote, source)
       tags' <- mapM (readOrInsert DBTag) tags
       mapM (associate (DBQuote, DBTag) . ( (,) idQuote)) tags'
       return ()

getPrimaryKey :: (Monad m) => DBType -> String -> FalliableSerialization r m Integer
getPrimaryKey typ s 
    = do result <- search typ (ByName s)
         case result of
            [] -> throwError $ NonExistingDataError s
            [dbv] -> return $ primaryKey dbv
            res -> throwError $ AmbiguousDataError $ map (show . value) res
       
validateAuthor :: (Monad m) => String -> FalliableSerialization r m Integer
validateAuthor s = do
                    result <- search DBAuthor (ByName s)
                    case result of
                        [] -> throwError $ NonExistingDataError s
                        [dbv] -> return $ primaryKey dbv
                        res -> throwError $ AmbiguousDataError $ map (show . value) res

{- Insert metadatas. In the Metadata table, insert the value,
and the primary key to the related Metadata Type and Source. -}
insertMetadatas :: (Monad m) => PrimaryKey -> (String, String) -> FalliableSerialization r m ()
insertMetadatas sourceId (info, value)
    = do pk <- readOrInsert DBMetadataInfo info
         associate2 (pk, sourceId) (PMetadataValue value)
         return ()

readOrInsert :: (Monad m) => DBType -> String -> FalliableSerialization r m PrimaryKey
readOrInsert typ term = do
                        result <- search typ (ByName term)
                        case result of
                            []  -> do
                                create $ (findConstructor typ) term
                                lastInsert
                            [dbv] -> return $ primaryKey dbv
                            dbvs -> throwError $ AmbiguousDataError $ map (show . value) dbvs
    where
        findConstructor DBMetadataInfo = PMetadataInfo
        findConstructor DBTag = PTag
