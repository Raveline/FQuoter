module FQuoter.Serialize.Queries 
(
    getInsert,
    getSearch,
    getAssociate
)
where

import FQuoter.Serialize.SerializedTypes

data QueryType
    = QInsert DBType
    | QSearch DBType SearchTerm
    | QUpdate DBType
    | QDelete DBType SearchTerm

getAssociate t = queryFor $ QInsert t

getInsert (SAuthor _) = queryFor (QInsert DBAuthor)
getInsert (SSource _) = queryFor (QInsert DBSource)
getInsert (SMetadataInfo _) = queryFor (QInsert DBMetadataInfo)

getSearch :: DBType -> SearchTerm -> Query
getSearch dbt st = queryFor $ QSearch dbt st

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
queryFor (QInsert DBMetadataValue) = "INSERT INTO MetadataValue VALUES (?,?,?)"
queryFor (QUpdate DBAuthor) = "UPDATE author SET first_name = ?, last_name = ?, surname = ? WHERE id_author = ?"
queryFor (QDelete DBAuthor (ById _)) = "DELETE FROM Author id_author = ?"
