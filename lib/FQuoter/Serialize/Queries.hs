module FQuoter.Serialize.Queries 
(
    getInsert,
    getSearch,
    getDelete,
    getAssociate,
    getAssociate2
)
where

import FQuoter.Parser.ParserTypes
import FQuoter.Serialize.SerializedTypes

baseQuoteSearch :: String
baseQuoteSearch = "SELECT q.id_quote, q.localization, q.content, q.comment \
     \  , s.title, mi.name, mv.value, t.name, a.first_name, a.last_name, \
     \ a.surname FROM Quote q\
     \ LEFT JOIN Source s ON q.related_source = s.id_source\
     \ LEFT JOIN Quote_Authors qa ON qa.related_quote = q.id_quote\
     \ LEFT JOIN MetadataValue mv ON q.related_source = s.id_source\
     \ LEFT JOIN MetadataInfo mi ON mv.related_metadata = mi.id_metadataInfo \
     \ LEFT JOIN Author a ON qa.related_author = a.id_author\
     \ LEFT JOIN Quote_Tags qt ON qt.related_quote = q.id_quote\
     \ LEFT JOIN Tag t ON t.id_tag = qt.related_tag"

data QueryType
    = QInsert DBType
    | QAssociate DBType DBType 
    | QSearch DBType SearchTerm
    | QUpdate DBType
    | QDelete DBType 
    deriving (Show)

getAssociate2 :: DBType -> Query
getAssociate2 t = queryFor $ QInsert t

getInsert :: ParsedType -> Query
getInsert (PAuthor _) = queryFor (QInsert DBAuthor)
getInsert (PSource _) = queryFor (QInsert DBSource)
getInsert (PMetadataInfo _) = queryFor (QInsert DBMetadataInfo)
getInsert (PLinkedQuote _) = queryFor (QInsert DBQuote)
getInsert (PTag _) = queryFor (QInsert DBTag)
getInsert (PMetadataValue _) = queryFor (QInsert DBMetadataValue)
getInsert _ = error "No queries for this type."

getDelete :: DBType -> Query
getDelete t = queryFor (QDelete t)

getSearch :: DBType -> SearchTerm -> Query
getSearch dbt st = queryFor $ QSearch dbt st

getAssociate :: DBType -> DBType -> Query
getAssociate t1 t2 = queryFor $ QAssociate t1 t2

queryFor :: QueryType -> Query
queryFor (QInsert DBAuthor) = "INSERT INTO Author VALUES (?,?,?,?)"
queryFor (QInsert DBSource) = "INSERT INTO Source VALUES (?, ?)"
queryFor (QInsert DBMetadataInfo) = "INSERT INTO MetadataInfo VALUES (?, ?)"
queryFor (QInsert DBTag) = "INSERT INTO Tag VALUES (?, ?)"
queryFor (QInsert DBMetadataValue) = "INSERT INTO MetadataValue VALUES (?,?,?,?)"
queryFor (QInsert DBQuote) = "INSERT INTO Quote VALUES (?,?,?,?,?)"
queryFor (QSearch DBAuthor (ByName _) ) 
    =  "SELECT * FROM Author \
       \WHERE first_name || \" \" || last_name like ? \
       \OR surname like ?"
queryFor (QSearch DBAuthor (ById _) ) 
    = "SELECT * FROM Author WHERE id_author = ?"
queryFor (QSearch DBMetadataInfo (ByName _))
    = "SELECT * FROM MetadataInfo WHERE  name like ?"
queryFor (QSearch DBTag (ByName _))
    = "SELECT * FROM Tag WHERE name like ?"
queryFor (QSearch DBSource (ByName _) ) 
    = "SELECT * FROM Source WHERE title like ?"
queryFor (QSearch DBQuote (ByName _) ) = baseQuoteSearch ++ " WHERE q.content like ?"
queryFor (QSearch DBQuote (ByIn _)) = baseQuoteSearch ++ " WHERE t.name IN (?)"
queryFor (QSearch DBAuthor (ByAssociation (DBSource, DBAuthor) _)) =
    "SELECT a.* FROM Source_Authors sa LEFT JOIN Author a\
    \ ON sa.related_author = a.id_author WHERE sa.related_source = ?"
queryFor (QUpdate DBAuthor) = "UPDATE author SET first_name = ?, last_name = ?, surname = ? WHERE id_author = ?"
queryFor (QAssociate DBSource DBAuthor) = "INSERT INTO Source_Authors VALUES (?,?,?)"
queryFor (QAssociate DBQuote DBAuthor) = "INSERT INTO Quote_Authors VALUES (?,?,?)"
queryFor (QAssociate DBQuote DBTag) = "INSERT INTO Quote_Tags VALUES (?,?,?)"
queryFor (QDelete DBQuote) = "DELETE FROM Quote WHERE id_quote = ?"
queryFor (QDelete DBAuthor) = "DELETE FROM Author WHERE id_author = ?"
queryFor (QDelete DBSource) = "DELETE FROM Source WHERE id_source = ?"
queryFor q = error $ "No query for : " ++ show q
