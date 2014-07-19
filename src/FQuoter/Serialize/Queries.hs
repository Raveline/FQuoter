module FQuoter.Serialize.Queries 
(
    getInsert,
    getSearch,
    getAssociate,
    getAssociate2
)
where

import FQuoter.Parser.ParserTypes
import FQuoter.Serialize.SerializedTypes

data QueryType
    = QInsert DBType
    | QAssociate DBType DBType 
    | QSearch DBType SearchTerm
    | QUpdate DBType
    | QDelete DBType SearchTerm
    deriving (Show)

getAssociate2 t = queryFor $ QInsert t

getInsert (PAuthor _) = queryFor (QInsert DBAuthor)
getInsert (PSource _) = queryFor (QInsert DBSource)
getInsert (PMetadataInfo _) = queryFor (QInsert DBMetadataInfo)
getInsert (PLinkedQuote _) = queryFor (QInsert DBQuote)
getInsert (PTag _) = queryFor (QInsert DBTag)

getSearch :: DBType -> SearchTerm -> Query
getSearch dbt st = queryFor $ QSearch dbt st

getAssociate :: DBType -> DBType -> Query
getAssociate t1 t2 = queryFor $ QAssociate t1 t2

queryFor :: QueryType -> Query
queryFor (QInsert DBAuthor) = "INSERT INTO Author VALUES (?,?,?,?)"
queryFor (QInsert DBSource) = "INSERT INTO Source VALUES (?, ?)"
queryFor (QInsert DBMetadataInfo) = "INSERT INTO MetadataInfo VALUES (?, ?)"
queryFor (QSearch DBAuthor (ByName _) ) 
    =  "SELECT * FROM Author \
       \WHERE first_name || \" \" || last_name like ? \
       \OR surname like ?"
queryFor (QSearch DBAuthor (ById _) ) 
    = "SELECT * FROM Author WHERE id_author = ?"
queryFor (QSearch DBMetadataInfo (ByName _))
    = "SELECT * FROM MetadataInfo WHERE  name like ?"
queryFor (QSearch DBSource (ByName _) ) 
    = "SELECT * FROM Source WHERE title like ?"
queryFor (QInsert DBMetadataValue) = "INSERT INTO MetadataValue VALUES (?,?,?)"
queryFor (QInsert DBTag) = "INSERT INTO Tag VALUES (?,?)"
queryFor (QInsert DBQuote) = "INSERT INTO Quote VALUES (?,?,?,?,?)"
queryFor (QUpdate DBAuthor) = "UPDATE author SET first_name = ?, last_name = ?, surname = ? WHERE id_author = ?"
queryFor (QDelete DBAuthor (ById _)) = "DELETE FROM Author id_author = ?"
queryFor (QAssociate DBSource DBAuthor) = "INSERT INTO Source_Authors VALUES (?,?,?)"
queryFor (QAssociate DBQuote DBAuthor) = "INSERT INTO Quote_Authors VALUES (?,?,?)"
queryFor (QAssociate DBQuote DBTag) = "INSERT INTO Quote_Tags VALUES (?,?,?)"
