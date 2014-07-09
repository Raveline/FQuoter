module FQuoter.Serialize.SerializedTypes
where

import FQuoter.Quote
import Database.HDBC
import qualified Data.Map as Map

---- TYPES
type PrimaryKey = Integer 
type Query = String 
type PairOfKeys = (PrimaryKey, PrimaryKey)

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

-- Anything that can be serialized must be translated in a list of
-- SqlValues, and can be translated back from it. We will however
-- stored those "translated" objects as DBValues, wich will contain
-- the primary key.
class SqliteSerializable a where
    sqlize :: a -> [SqlValue]
    unsqlize :: [SqlValue] -> DBValue a

{- Container for serialized type. Can contain all the basic Quoter types.
Is used to insert them and as a return for queries.
Allow for the type system to pick the proper queries. -}
data SerializedType 
    = SAuthor Author
    | SSource Source
    | SQuote Quote
    | SMetadataInfo MetadataInfo
    | SMetadataValue MetadataValue

-- A simple container to hold a data type and
-- its primary key in the database.
data DBValue a = DBValue { primary_key :: PrimaryKey
                                     , value :: a
                         }

instance Functor DBValue where
    fmap f (DBValue pk v) = DBValue pk (f v)


instance SqliteSerializable Author where
    sqlize (Author fName lName sName) = [SqlNull
                                        ,maybeStringToSql fName
                                        ,maybeStringToSql lName
                                        ,maybeStringToSql sName]
    unsqlize (pkey:fName:lName:sName:[]) = DBValue (fromSql pkey) toAuthor
        where
            toAuthor = Author (sqlToMaybeString fName)
                                 (sqlToMaybeString lName)
                                 (sqlToMaybeString sName)

instance SqliteSerializable Source where
    sqlize (Source title _ _) = [SqlNull
                                ,toSql title]
    unsqlize (pkey:title:[]) =  DBValue (fromSql pkey) 
                                (Source (fromSql title) [] Map.empty)

instance SqliteSerializable QuoterString where
    sqlize s = [SqlNull, toSql (string s)]
    unsqlize (pkey:s:[]) = DBValue (fromSql pkey) (QuoterString(fromSql s))

instance SqliteSerializable Quote where
    sqlize = undefined
    unsqlize = undefined

instance SqliteSerializable MetadataInfo where
    sqlize = sqlize . metaInfo
    unsqlize = fmap MetadataInfo . unsqlize 

instance SqliteSerializable MetadataValue where
    sqlize = sqlize . metaValue
    unsqlize = fmap MetadataValue . unsqlize

sqlizeST :: SerializedType -> [SqlValue]
sqlizeST (SAuthor a) = sqlize a
sqlizeST (SSource s) = sqlize s
sqlizeST (SQuote q) = sqlize q
sqlizeST (SMetadataInfo i) = sqlize i
sqlizeST (SMetadataValue v) = sqlize v

unsqlizeST :: DBType -> [SqlValue] -> DBValue SerializedType
unsqlizeST (DBAuthor) = fmap SAuthor . unsqlize
unsqlizeST (DBSource) = fmap SSource . unsqlize
unsqlizeST (DBQuote) = fmap SQuote . unsqlize
unsqlizeST (DBMetadataInfo) = fmap SMetadataInfo . unsqlize
unsqlizeST (DBMetadataValue) = fmap SMetadataValue . unsqlize

data SearchTerm 
    = ById Integer
    | ByName String

data DBType = DBAuthor | DBSource | DBMetadataInfo | DBMetadataValue | DBQuote
