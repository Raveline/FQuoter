{- This module handle <temporary> types, buffer from user input to
"real" types. -}

module FQuoter.Parser.ParserTypes where 

import FQuoter.Quote
import Data.Map

type PMetadataDictionary = Map String String

{- If the user does not want to type everything in the command-line,
 - we will just get a "NotDefinedType", that will prompt a series of
 - questions to defined our type. -}
data NotDefinedType
    = NDAuthor
    | NDSource
    | NDQuote
    deriving (Eq, Show)

data ParsedType
    = PAuthor Author
    | PSource ParserSource
    | PMetadataInfo String
    | PMetadataValue String
    | PQuote ParserQuote
    | PLinkedQuote LinkedParserQuote
    | PTag String
    deriving (Eq, Show)

data ParserSource = ParserSource { prsTitle :: String
                                 , prsAuthors :: [String]
                                 , prsMetadata :: Map String String }
                                 deriving (Eq, Show)

{- This is the parsed quote, as inputed by an user. -}
data ParserQuote = ParserQuote { prsContent :: String
                               , prsSource :: String
                               , prsLocalization :: Maybe String
                               , prsTags :: [String]
                               , prsAuthors' :: [String]
                               , prsComment :: Maybe String }
                               deriving (Eq, Show)

{- This is the linked quote, with the source id and the
id of authors. -}
data LinkedParserQuote = LinkedQuote { prsQuote :: ParserQuote
                                     ,  sourcePk :: Integer }
                                       deriving (Eq, Show)

data TypeProperty = ModifyAuthor AuthorProperty
                    | ModifySource SourceProperty
                    | ModifyQuote QuoteProperty
                    deriving (Eq, Show)

data AuthorProperty = AuthorFirstName (Maybe String)
                      | AuthorLastName (Maybe String)
                      | AuthorNickName (Maybe String)
                      deriving (Eq, Show)

data SourceProperty = SourceTitle String
                      | SourceMetadata MetadataInfo (Maybe MetadataValue)
                      | SourceAuthors [Author]
                      deriving (Eq, Show)

data QuoteProperty = QuoteContent String
                     | QuoteComment (Maybe String)
                     | QuoteLocation (Maybe String)
                     | QuoteTags [String]
                     | QuoteAuthors [String]
                     | QuoteSource String
                     deriving (Eq, Show)
