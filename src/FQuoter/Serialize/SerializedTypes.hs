module FQuoter.Serialize.SerializedTypes
where

import FQuoter.Quote
import FQuoter.Parser.ParserTypes
import Database.HDBC
import qualified Data.Map as Map
import Data.List.Split

---- TYPES
type PrimaryKey = Integer 
type Query = String 
type PairOfKeys = (PrimaryKey, PrimaryKey)
type PairOfTypes = (DBType, DBType)

-- Conversion utilities
maybeStringToSql :: Maybe String -> SqlValue
maybeStringToSql Nothing        = toSql ""
maybeStringToSql (Just s)       = toSql s

sqlToMaybeString :: SqlValue -> Maybe String
sqlToMaybeString s@(SqlByteString _)  = stringToMaybe $ fromSql s 
    where
        stringToMaybe :: String -> Maybe String
        stringToMaybe [] = Nothing
        stringToMaybe s = Just s
sqlToMaybeString _ = Nothing

{- Container for serialized type. Can contain all the basic Quoter types.
Is used to insert them and as a return for queries.
Allow for the type system to pick the proper queries. -}
data SerializedType 
    = SAuthor Author
    | SSource Source
    | SQuote Quote
    | SMetadataInfo MetadataInfo
    | SMetadataValue MetadataValue
    deriving(Show)

-- A simple container to hold a data type and
-- its primary key in the database.
data DBValue a = DBValue { primaryKey :: PrimaryKey
                                     , value :: a
                         }


sqlize :: ParsedType -> [SqlValue]
sqlize (PAuthor (Author fName lName sName)) = [SqlNull
                                              ,maybeStringToSql fName
                                              ,maybeStringToSql lName
                                              ,maybeStringToSql sName]
sqlize (PSource (ParserSource title _ _)) =  [SqlNull
                                             ,toSql title]
sqlize (PMetadataInfo s) = sqlizeQuoterString s
sqlize (PMetadataValue v) = sqlizeQuoterString v
sqlize (PQuote _) = 
    error "Wrong call. Link with source first and use PLinkedQuote."
sqlize (PLinkedQuote (LinkedQuote (ParserQuote txt _ loc _ _ comm) source)) = 
    [SqlNull
    ,toSql source
    ,maybeStringToSql loc
    ,toSql txt
    ,maybeStringToSql comm]
sqlize (PTag tag) = sqlizeQuoterString tag

sqlizeQuoterString s = [SqlNull, toSql s]

unsqlizeST :: DBType -> [SqlValue] -> DBValue SerializedType
unsqlizeST DBAuthor (pkey:fName:lName:sName:[]) = 
    DBValue (fromSql pkey) (SAuthor $ sqlValuesToAuthor fName lName sName)
unsqlizeST DBAuthor _ = error $ "SQL error, result array not fitting for author."
unsqlizeST DBSource (pkey:title:[]) = 
    DBValue (fromSql pkey) (SSource $ sqlValuesToSource title)
unsqlizeST DBSource _ = error $ "SQL error, result array not fitting for source."
unsqlizeST DBQuote (pk:loc:cont:comm:title:afname:alname:anname:ltags:[]) = 
    DBValue (fromSql pk)
            (SQuote quote')
        where author' = sqlValuesToAuthor afname alname anname
              source' = Source (fromSql title) [] Map.empty
              toTagList = splitOn "," . fromSql $ ltags
              quote' = Quote author' source' (fromSql cont) (sqlToMaybeString loc) (toTagList) (sqlToMaybeString comm)
unsqlizeST DBMetadataInfo (key:s:[]) = 
    DBValue (fromSql key) (SMetadataInfo $ MetadataInfo $ sqlValuesToQuoterString s)
unsqlizeST DBMetadataValue (key:s:[]) =
    DBValue (fromSql key) (SMetadataValue $ MetadataValue $ sqlValuesToQuoterString s)

sqlValuesToAuthor :: SqlValue -> SqlValue -> SqlValue -> Author
sqlValuesToAuthor fname lname nname = Author (sqlToMaybeString fname)
                                             (sqlToMaybeString lname)
                                             (sqlToMaybeString nname)

sqlValuesToQuoterString :: SqlValue -> QuoterString
sqlValuesToQuoterString = QuoterString . fromSql

sqlValuesToSource :: SqlValue -> Source
sqlValuesToSource title = Source (fromSql title) [] Map.empty

data SearchTerm 
    = ById Integer
    | ByName String
    | ByAssociation PairOfTypes Integer
    deriving (Eq, Show)

data DBType = DBAuthor | DBSource | DBMetadataInfo | DBMetadataValue | DBQuote | DBTag
    deriving (Eq, Show)
