module FQuoter.Serialize.Shortcuts
(
    insert
)
where

import qualified Data.Map as Map
import Data.Either

import FQuoter.Serialize.Serialize
import FQuoter.Serialize.SerializedTypes
import FQuoter.Parser.ParserTypes
import FQuoter.Serialize.Queries
import FQuoter.Quote

type LinkingAttempt = Either DBError Integer

insert :: ParsedType -> Serialization DBActionResult
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
       case partitionEithers validatedAuthors of
            ([], good) -> do
                    -- Do something with good authors
                    return $ Map.mapWithKey (insertMetadatas idSource) meta
                    return $ Right "Ok !"
            ([xs], _) -> return $ Left xs
       
validateAuthor :: String -> Serialization LinkingAttempt
validateAuthor s = do
                    result <- search DBAuthor (ByName s)
                    return (case result of
                        [] -> Left $ UnexistingData "Non existing author."
                        [dbv] -> Right $ primaryKey dbv
                        res -> Left $ AmbiguousData ("Several potential authors for " ++ s) (map (show . value) res))

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

