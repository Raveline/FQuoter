module FQuoter.Quote where

import qualified Data.Map as Map

type Tag = String

newtype QuoterString = QuoterString { string :: String } deriving (Eq, Show, Ord)
-- The abstract idea of a metadata ("Editor", "Publication date", etc.)
newtype MetadataInfo = MetadataInfo { metaInfo :: QuoterString } deriving (Eq, Show, Ord)
metadataInfo :: MetadataInfo -> String
metadataInfo = string . metaInfo
-- The concrete value of a metadata ("Everyman's", "1992", etc.)
newtype MetadataValue = MetadataValue { metaValue :: QuoterString } deriving (Eq, Show, Ord)
metadataValue :: MetadataValue -> String
metadataValue = string . metaValue

type MetadataDictionary = Map.Map MetadataInfo MetadataValue

anonymous :: String
anonymous = "Anonymous"

data Author = Author { firstName :: Maybe String
                     , lastName :: Maybe String
                     , surname :: Maybe String
                     } deriving (Eq)

data Source = Source  { title :: String
                      , authors :: [Author]
                      , metadata :: MetadataDictionary
                      } deriving (Eq)

data Quote = Quote { author :: [Author]
                   , source :: Source
                   , content :: String
                   , location :: Maybe String
                   , tags :: [Tag]
                   , comment :: Maybe String
                   } deriving (Eq)

mainAuthor :: Quote -> Author
mainAuthor = head . author

lookupMetadata :: String -> Source -> Maybe String
lookupMetadata s = fmap metadataValue . Map.lookup s' . metadata
    where s' = MetadataInfo . QuoterString $ s

instance Show Author where
    show (Author (Just f) (Just l) Nothing) = f ++ " " ++ l
    show (Author _ _ (Just s)) = s
    show _ = anonymous

instance Show Source where
    show (Source t _ _) = t

instance Show Quote where
    show q = show (author q) ++ " - " ++ content q
