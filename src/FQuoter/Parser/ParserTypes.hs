{- This module handle <temporary> types, buffer from user input to
"real" types. -}

module FQuoter.Parser.ParserTypes where 

import FQuoter.Quote
import Data.Map

type PMetadataDictionary = Map String String

data ParsedType
    = PAuthor Author
    | PSource ParserSource
    | PMetadataInfo String
    | PMetadataValue String
    | PQuote ParserQuote
    deriving (Eq, Show)

data ParserSource = ParserSource { ptitle :: String
                                 , pauthors :: [String]
                                 , pmetadata :: Map String String }
                                 deriving (Eq, Show)

data ParserQuote = ParserQuote { content :: String,
                                 comment :: String,
                                 source :: String,
                                 tags :: [String] }
                                 deriving (Eq, Show)
