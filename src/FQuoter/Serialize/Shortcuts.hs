module FQuoter.Serialize.Shortcuts
(
    insert
)
where

import FQuoter.Serialize.Serialize
import FQuoter.Serialize.SerializedTypes
import FQuoter.Parser.ParserTypes
import FQuoter.Serialize.Queries
import FQuoter.Quote
import qualified Data.Map as Map

{-- Return values for validation functions.
The idea is to check that a string can be matched
to something in the database.
e.g. : "Insert "War and Peace" by Tolstoi" will first
check that "Tolstoi" can be associated to an author in DB. -}
data Prevalidation a
    = Good a             -- Validated input
    | Nonexisting String -- Data does not exist in DB
    | Ambiguous [String] -- Ambiguous request : many potential data

insert :: ParsedType -> Serialization ()
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
       authorsId <- validateAuthors auth
       return $ Map.mapWithKey (insertMetadatas idSource) meta
       return ()

validateAuthors :: [String] -> Serialization [Integer]
validateAuthors = mapM validateAuthor

validateAuthor :: String -> Serialization Integer
validateAuthor s = do
                    result <- search DBAuthor (ByName s)
                    case result of
                        [] -> error "Non existing author."
                        [dbv] -> return $ primaryKey dbv
                        _ -> error "Ambiguous case, not handled yet."

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

