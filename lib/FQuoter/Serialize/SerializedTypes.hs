{-# LANGUAGE FlexibleContexts #-}
module FQuoter.Serialize.SerializedTypes
where

import FQuoter.Quote
import Data.Convertible
import FQuoter.Parser.ParserTypes
import Database.HDBC
import qualified Data.Map as Map

---- TYPES
type PrimaryKey = Integer 
type Query = String 
type PairOfKeys = (PrimaryKey, PrimaryKey)
type PairOfTypes = (DBType, DBType)

data SqlOutput = Single SqlValue
               | Grouped [SqlOutput]
               deriving (Eq, Show)

data SearchTerm 
    = ById Integer
    | ByName String
    | ByIn [String]
    | ByAssociation PairOfTypes Integer
    deriving (Eq, Show)

data DBType = DBAuthor | DBSource | DBMetadataInfo | DBMetadataValue | DBQuote | DBTag
    deriving (Eq, Show, Ord)

{- Container for serialized type. Can contain all the basic Quoter types.
Is used to insert them and as a return for queries.
Allow for the type system to pick the proper queries. -}
data SerializedType 
    = SAuthor Author
    | SSource Source
    | SQuote Quote
    | SMetadataInfo MetadataInfo
    | SMetadataValue MetadataValue
    deriving(Show, Eq)

-- A simple container to hold a data type and
-- its primary key in the database.
data DBValue a = DBValue { primaryKey :: PrimaryKey
                                     , value :: a
                         }
-- Conversion utilities
maybeStringToSql :: Maybe String -> SqlValue
maybeStringToSql Nothing        = toSql ""
maybeStringToSql (Just s)       = toSql s

sqlOutputToMaybeString :: SqlOutput -> Maybe String
sqlOutputToMaybeString (Single v) = sqlToMaybeString v
sqlOutputToMaybeString (Grouped _) = error "Cannot make a string out of a group"

sqlToMaybeString :: SqlValue -> Maybe String
sqlToMaybeString s@(SqlByteString _)  = stringToMaybe $ fromSql s 
    where
        stringToMaybe :: String -> Maybe String
        stringToMaybe [] = Nothing
        stringToMaybe s' = Just s'
sqlToMaybeString _ = Nothing

fromSql' :: Convertible SqlValue a => SqlOutput -> a
fromSql' (Single x) = fromSql x
fromSql' (Grouped _) = error "Cannot convert group."

sqlize :: ParsedType -> [SqlValue]
sqlize (PAuthor (Author fName lName sName)) = [maybeStringToSql fName
                                              ,maybeStringToSql lName
                                              ,maybeStringToSql sName]
sqlize (PSource (ParserSource t _ _)) =  [toSql t]
sqlize (PMetadataInfo s) = sqlizeQuoterString s
sqlize (PMetadataValue v) = sqlizeQuoterString v
sqlize (PQuote _) = 
    error "Wrong call. Link with source first and use PLinkedQuote."
sqlize (PLinkedQuote (LinkedQuote (ParserQuote txt _ loc _ _ comm) source')) = 
    [toSql source'
    ,maybeStringToSql loc
    ,toSql txt
    ,maybeStringToSql comm]
sqlize (PTag tag) = sqlizeQuoterString tag

sqlizeQuoterString :: String -> [SqlValue]
sqlizeQuoterString s = [toSql s]

unsqlizeST :: DBType -> [SqlOutput] -> DBValue SerializedType
unsqlizeST DBAuthor [pkey,fName,lName,sName] = 
    DBValue (fromSql' pkey) (SAuthor $ sqlOutputsToAuthor fName lName sName)
unsqlizeST DBAuthor _ = error "SQL error, result array not fitting for author."
unsqlizeST DBSource [pk,t] = 
    DBValue (fromSql' pk) (SSource $ sqlValuesToSource t)
unsqlizeST DBSource _ = error "SQL error, result array not fitting for source."
unsqlizeST DBQuote [Grouped meta,Grouped tags',Grouped auths, pk, loc, cont, comm, tit] = 
    DBValue (fromSql' pk)
            (SQuote quote')
        where authors' = map groupToAuthor auths
              metadatas' = buildMap meta
              source' = Source (fromSql' tit) [] metadatas' 
              toTagList = map fromSql' . filter (/= Single SqlNull) $ tags'
              quote' = Quote authors' source' (fromSql' cont) (sqlOutputToMaybeString loc) toTagList (sqlOutputToMaybeString comm)
unsqlizeST DBMetadataInfo [key,s] = 
    DBValue (fromSql' key) (SMetadataInfo $ MetadataInfo $ sqlValuesToQuoterString s)
unsqlizeST DBMetadataValue [key,s] = 
    DBValue (fromSql' key) (SMetadataValue $ MetadataValue $ sqlValuesToQuoterString s)
unsqlizeST t xs = error $ "Couldn't handle " ++ show t ++ " with values : " ++ show xs

buildMap :: [SqlOutput] -> MetadataDictionary
buildMap = Map.fromList . map toMetaPairs . onlyResult . buildMap'
    where
        onlyResult :: [(SqlValue, SqlValue)] -> [(SqlValue, SqlValue)]
        onlyResult = filter (/= (SqlNull, SqlNull))
        toMetaPairs (a,b) = (MetadataInfo . QuoterString . fromSql $ a
                            ,MetadataValue . QuoterString . fromSql $ b)
        buildMap' :: [SqlOutput] -> [(SqlValue, SqlValue)]
        buildMap' = map fromGroupedToPair 
        fromGroupedToPair :: SqlOutput -> (SqlValue, SqlValue)
        fromGroupedToPair (Grouped [Single a, Single b]) = (a,b)
        fromGroupedToPair _ = error "Error in grouping, cannot build metadata dictionary."

groupToAuthor :: SqlOutput -> Author
groupToAuthor (Grouped [fn,ls,nn]) = sqlOutputsToAuthor fn ls nn
groupToAuthor _ = error "Uncorrect grouping for author."

sqlOutputsToAuthor :: SqlOutput -> SqlOutput -> SqlOutput -> Author
sqlOutputsToAuthor fname lname nname = Author (sqlOutputToMaybeString fname)
                                             (sqlOutputToMaybeString lname)
                                             (sqlOutputToMaybeString nname)

sqlValuesToQuoterString :: SqlOutput-> QuoterString
sqlValuesToQuoterString = QuoterString . fromSql'

sqlValuesToSource :: SqlOutput -> Source
sqlValuesToSource t = Source (fromSql' t) [] Map.empty
