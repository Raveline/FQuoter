{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
module FQuoter.Serialize.Shortcuts
(
    DBError (..),
    insert,
    searchWord,
    searchTags,
    remove,
    FalliableSerialization
)
where

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Trans.Free
import Control.Monad.Reader

import FQuoter.Serialize.Serialize
import FQuoter.Serialize.SerializedTypes
import FQuoter.Parser.ParserTypes

data DBError = NonExistingDataError String
             | AmbiguousDataError String [SerializedType]
             deriving(Eq)

instance Show DBError where
    show (NonExistingDataError s) = "Input " ++ s ++ " could not be matched in database."
    show (AmbiguousDataError s sts) = 
        "Ambiguous input : " ++ s ++ ".\nCannot choose between :\n" ++ (unlines . map show $ sts)

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
insert s@(PSource (ParserSource _ auth meta)) = 
    do create s
       idSource <- lastInsert 
       validatedAuthors <- mapM validateAuthor auth
       mapM_ (associate (DBSource, DBAuthor) . (,) idSource) validatedAuthors
       mapM_ (insertMetadatas idSource) (Map.toList meta)
       return ()
{- Insert an author in the DB. This... takes many steps.
First we need to find, if it exists, the related source.
Then, we want to find, if it has been specified, the author.
If the user did not specify the author, we'll go for the default
one (the source's author or authors). Then, we need to make a
specific object with the proper id of the source.
And then, we need to associate authors and tags, through the
proper tables, to this source. -}
insert (PQuote pq@(ParserQuote _ source _ tags auths _)) =
    do source <- getPrimaryKey DBSource source
       let insertableQuote = PLinkedQuote $ LinkedQuote pq source
       create insertableQuote
       idQuote <- lastInsert
       authors <- getSourceAuthors auths source
       mapM_ (associate (DBQuote, DBAuthor) . (,) idQuote) authors
       tags' <- mapM (readOrInsert DBTag) tags
       mapM_ (associate (DBQuote, DBTag) . (,) idQuote) tags'
insert _ = error "Only Quote, Source and Authors can be directly inserted."

{- Get the authors of a given source. This function is mainly used
associate authors to a quote when no specific author was given. -}
getSourceAuthors :: (Monad m) => [String] -> Integer -> FalliableSerialization r m [Integer]
getSourceAuthors [] idSource = 
    search DBAuthor (ByAssociation (DBSource, DBAuthor) idSource)
    >>= mapM (return . primaryKey) 
getSourceAuthors xs _        = mapM validateAuthor xs

{- Search the database with a ByName query and return only one result.
Fail with error if several results were returned. -}
searchOne :: (Monad m) => DBType -> String -> FalliableSerialization r m (DBValue SerializedType)
searchOne typ s = do result <- search typ (ByName s)
                     case result of
                         [] -> throwError $ NonExistingDataError s
                         [dbv] -> return dbv
                         res -> throwError $ AmbiguousDataError s $ map value res

getPrimaryKey :: (Monad m) => DBType -> String -> FalliableSerialization r m Integer
getPrimaryKey typ s = searchOne typ s >>= return . primaryKey
       
validateAuthor :: (Monad m) => String -> FalliableSerialization r m Integer
validateAuthor s = do
                    result <- search DBAuthor (ByName s)
                    case result of
                        [] -> throwError $ NonExistingDataError s
                        [dbv] -> return $ primaryKey dbv
                        res -> throwError $ AmbiguousDataError s $ map value res

{- Searching throuh word will return a list of quotes,
boxed in SerializedType. -}
searchWord :: (Monad m) => String -> FalliableSerialization r m [SerializedType]
searchWord w = do v <- search DBQuote (ByName w)
                  mapM (return . value) v

searchTags :: (Monad m) => [String] -> FalliableSerialization r m [SerializedType]
searchTags ws = do v <- search DBQuote (ByIn ws)
                   mapM (return . value) v

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
                                create $ findConstructor typ term
                                lastInsert
                            [dbv] -> return $ primaryKey dbv
                            dbvs -> throwError $ AmbiguousDataError term $ map value dbvs
    where
        findConstructor DBMetadataInfo = PMetadataInfo
        findConstructor DBTag = PTag
        findConstructor _ = error "Only used for MetadataInfo and Tag."

remove :: (Monad m) => DBType -> String -> FalliableSerialization r m SerializedType
remove typ s = do res <- searchOne typ s
                  delete typ $ primaryKey res
                  return $ value res
