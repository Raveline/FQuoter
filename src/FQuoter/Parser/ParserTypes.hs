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

data ParserSource = ParserSource { prsTitle :: String
                                 , prsAuthors :: [String]
                                 , prsMetadata :: Map String String }
                                 deriving (Eq, Show)

data ParserQuote = ParserQuote { prsContent :: String,
                                 prsSource :: String,
                                 prsTags :: [String],
                                 prsLocalization :: Maybe String,
                                 prsComment :: Maybe String }
                                 deriving (Eq, Show)
