module FQuoter.Serialize.SerializedTypes
where

import FQuoter.Quote
import FQuoter.Parser.ParserTypes
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
    deriving(Show)

-- A simple container to hold a data type and
-- its primary key in the database.
data DBValue a = DBValue { primaryKey :: PrimaryKey
                                     , value :: a
                         }

instance Functor DBValue where
    fmap f (DBValue pk v) = DBValue pk (f v)


instance SqliteSerializable Author where
    unsqlize (pkey:fName:lName:sName:[]) = DBValue (fromSql pkey) toAuthor
        where
            toAuthor = Author (sqlToMaybeString fName)
                                 (sqlToMaybeString lName)
                                 (sqlToMaybeString sName)

instance SqliteSerializable Source where
    unsqlize (pkey:title:[]) =  DBValue (fromSql pkey) 
                                (Source (fromSql title) [] Map.empty)

instance SqliteSerializable QuoterString where
    unsqlize (pkey:s:[]) = DBValue (fromSql pkey) (QuoterString(fromSql s))

instance SqliteSerializable Quote where
    unsqlize = undefined

instance SqliteSerializable MetadataInfo where
    unsqlize = fmap MetadataInfo . unsqlize 

instance SqliteSerializable MetadataValue where
    unsqlize = fmap MetadataValue . unsqlize

sqlize :: ParsedType -> [SqlValue]
sqlize (PAuthor (Author fName lName sName)) = [SqlNull
                                              ,maybeStringToSql fName
                                              ,maybeStringToSql lName
                                              ,maybeStringToSql sName]
sqlize (PSource (ParserSource title _ _)) =  [SqlNull
                                             ,toSql title]
sqlize (PMetadataInfo s) = sqlizeQuoterString s
sqlize (PMetadataValue v) = sqlizeQuoterString v
sqlize (PQuote (ParserQuote txt _ _ loc comm)) = [SqlNull
                                                 ,toSql txt
                                                 ,maybeStringToSql loc
                                                 ,maybeStringToSql comm]

sqlizeQuoterString s = [SqlNull, toSql s]


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
