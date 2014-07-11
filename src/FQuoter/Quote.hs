module FQuoter.Quote where

import qualified Data.Map as Map

type Tag = String

newtype QuoterString = QuoterString { string :: String } deriving (Eq, Show, Ord)
-- The abstract idea of a metadata ("Editor", "Publication date", etc.)
newtype MetadataInfo = MetadataInfo { metaInfo :: QuoterString } deriving (Eq, Show, Ord)
metadataInfo = string . metaInfo
-- The concrete value of a metadata ("Everyman's", "1992", etc.)
newtype MetadataValue = MetadataValue { metaValue :: QuoterString } deriving (Eq, Show, Ord)
metadataValue = string . metaValue

type MetadataDictionary = Map.Map MetadataInfo MetadataValue

anonymous = "Anonymous"

data Author = Author { firstName :: Maybe String
                     , lastName :: Maybe String
                     , surname :: Maybe String
                     } deriving (Eq)

data Source = Source  { title :: String
                      , authors :: [Author]
                      , metadata :: MetadataDictionary
                      } deriving (Eq)

data Quote = Quote { author :: Author
                   , content :: String
                   , location :: String
                   , tags :: [Tag]
                   , comment :: String
                   } deriving (Eq)

instance Show Author where
    show (Author (Just f) (Just l) Nothing) = f ++ " " ++ l
    show (Author _ _ (Just s)) = s
    show _ = anonymous

instance Show Source where
    show (Source t _ _) = t

instance Show Quote where
    show (Quote _ c _ _ _) = c
