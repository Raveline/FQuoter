module FQuoter.Actions
where

import FQuoter.Parser.ParserTypes
import FQuoter.Serialize.SerializedTypes

data Action 
    = Insert (Either NotDefinedType ParsedType)
    | FindWord String
    | FindTags [String]
    | Remove DBType String
    | Updating DBType String Update TypeProperty
    | Shell
    deriving (Eq, Show)

data Update = Add
             | Set
             | Delete
             deriving (Eq, Show)
