{-# LANGUAGE FlexibleContexts #-}
module FQuoter.Serialize.Shortcuts
(
    insert
)
where

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.Free

import FQuoter.Serialize.Serialize
import FQuoter.Serialize.SerializedTypes
import FQuoter.Parser.ParserTypes
import FQuoter.Serialize.Queries
import FQuoter.Quote

insert :: ParsedType -> FalliableSerialization ()
{- Simply insert an author in the DB. -}
insert a@(PAuthor _) = lift $ create a
{- Insert a source in the DB. This is a three step
relation : first, insert the source in the Source table.
Then insert the link between Authors and the source.
Then, try to insert the metada. If the metadata are ambiguous,
an error is reported and the process is stopped.
NB: not fully implemented yet.
 -}
insert s@(PSource (ParserSource tit auth meta)) = 
    do lift $ create s
       idSource <- lift $ lastInsert 
       validatedAuthors <- mapM validateAuthor auth
       -- Do something with good authors
       return $ Map.mapWithKey (insertMetadatas idSource) meta
       return ()
       
validateAuthor :: String -> FalliableSerialization Integer
validateAuthor s = do
                    result <- lift $ search DBAuthor (ByName s)
                    case result of
                        [] -> throwError $ NonExistingDataError s
                        [dbv] -> return $ primaryKey dbv
                        res -> throwError $ AmbiguousDataError $ (map (show . value) res)

{- Insert metadatas. In the Metadata table, insert the value,
and the primary key to the related Metadata Type and Source. -}
insertMetadatas :: PrimaryKey -> String -> String -> Serialization ()
insertMetadatas sourceId info value 
    = do pk <- readOrInsert DBMetadataInfo info
         associate2 (pk, sourceId) (PMetadataValue value)
         return ()

{- Currently deficient function - not generalized enough. -}
readOrInsert :: DBType -> String -> Serialization PrimaryKey
readOrInsert typ term = do
                        result <- search typ (ByName term)
                        case result of
                            []  -> do
                                create $ PMetadataInfo term
                                lastInsert
                            [dbv] -> return $ primaryKey dbv
                            _   -> error "Not handling multiple results."

