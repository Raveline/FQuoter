module FQuoter.Quote where

import qualified Data.Map as Map

type Tag = String
-- The abstract idea of a metadata ("Editor", "Publication date", etc.)
type MetadataInfo = String
-- The concrete value of a metadata ("Everyman's", "1992", etc.)
type MetadataValue = String

type MetadataDictionary = Map.Map MetadataInfo MetadataValue

anonymous = "Anonymous"

data Author = Author { first_name :: Maybe String
                     , last_name :: Maybe String
                     , surname :: Maybe String
                     }

data Source = Source  { title :: String
                      , authors :: [Author]
                      , metadata :: MetadataDictionary
                      }

data Quote = Quote { author :: Author
                   , content :: String
                   , location :: String
                   , tags :: [Tag]
                   , comment :: String
                   }

instance Show Author where
    show (Author (Just f) (Just l) Nothing) = f ++ " " ++ l
    show (Author _ _ (Just s)) = s
    show _ = anonymous
