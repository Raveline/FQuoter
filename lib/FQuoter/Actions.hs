module FQuoter.Actions
where

import FQuoter.Parser.ParserTypes
import FQuoter.Serialize.SerializedTypes

data Action 
    = Insert (Either NotDefinedType ParsedType)
    | FindWord String
    | FindTags [String]
    | Remove DBType String
    | Update String 
    deriving (Eq, Show)

data Update = Add
             | Set
             | Delete
