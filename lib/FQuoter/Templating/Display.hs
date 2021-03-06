module FQuoter.Templating.Display
( readTree )
where

import Data.Maybe
import Control.Applicative
import Data.Monoid hiding (All)
import Data.Char

import FQuoter.Quote
import FQuoter.Templating.TemplateTypes

type Template = [TokenNode]

-- Given a tree template to display a quote and its context,
-- evaluate this tree with the quote.
readTree :: Template -> Quote -> String
readTree q s = fromMaybe "" $ mapSeveralNodes evalNode q s

-- Evaluate a single node in the display tree.
-- If an Author is detected, an helper function will be called
-- for there are several special cases concerning Authors.
evalNode :: TokenNode -> Quote -> Maybe String
evalNode (One mods (SourceInfo SourceTitle)) 
    = applyMods' mods . Just . title . source
evalNode (One mods (SourceInfo (SourceMetadata t))) 
    = applyMods' mods . lookupMetadata t . source
evalNode t@(One _ (AuthorInfo _)) = evalAuthor t . mainAuthor
evalNode (SomeAuthors All ts) 
    = mconcat . mapNodesAndItems evalAuthor ts . author
evalNode (SomeAuthors (Only n) ts) 
    = mconcat . take n . mapNodesAndItems evalAuthor ts . author
evalNode (One mods (ConstantString s)) = displayConstantString mods s
evalNode (Condition con tn) = handleCondition con tn
evalNode (Or tn tn') = orNode evalNode tn tn'

handleCondition :: TokenContent -> [TokenNode] -> Quote -> Maybe String
handleCondition (SourceInfo (SourceMetadata m)) tn q
    | isNothing (lookupMetadata m $ source q) = Nothing
    | otherwise = mapSeveralNodes evalNode tn q
handleCondition (AuthorInfo ai) tn q
    = handleAuthorCondition ai tn $ mainAuthor q
handleCondition _ _ _ 
    = error "SourceInfo or ConstantString cannot be conditions."

-- Helper function to display constants in templates
displayConstantString :: [TokenModificator] -> String -> a -> Maybe String
displayConstantString mods s _ = applyMods' mods . Just $ s

-- Helper function handling display commands for Authors
evalAuthor :: TokenNode -> Author -> Maybe String
evalAuthor (One mods (AuthorInfo ai)) = applyMods' mods . evalAuthorToken ai
evalAuthor (One mods (ConstantString s)) = displayConstantString mods s
evalAuthor (Or tn tn') = orNode evalAuthor tn tn'
evalAuthor (Condition (AuthorInfo ai) tn) = handleAuthorCondition ai tn
evalAuthor _ 
    = error "One with authorInfo, Or and Conditions only when evaluating this author "

handleAuthorCondition :: TokenAuthorInfo 
                        -> [TokenNode] 
                        -> Author 
                        -> Maybe String
handleAuthorCondition ai tn auth
    | isNothing $ evalAuthorToken ai auth = Nothing
    | otherwise = mapSeveralNodes evalAuthor tn auth

evalAuthorToken :: TokenAuthorInfo -> Author -> Maybe String
evalAuthorToken (AuthorNickName)    = surname
evalAuthorToken (AuthorLastName)    = lastName
evalAuthorToken (AuthorFirstName)   = firstName

mapNodesAndItems :: (TokenNode -> a -> Maybe String) -> [TokenNode] -> [a] -> [Maybe String]
mapNodesAndItems f ts = map (mapSeveralNodes f ts)

mapSeveralNodes :: (TokenNode -> a -> Maybe String) -> [TokenNode] -> a -> Maybe String
mapSeveralNodes f t a = mconcat . map (`f` a) $ t

orNode :: (TokenNode -> a -> Maybe String) -> [TokenNode] -> [TokenNode] -> a -> Maybe String
orNode f n1 n2 a = mapSeveralNodes f n1 a <|>  mapSeveralNodes f n2 a
    
applyMods' :: [TokenModificator] -> Maybe String -> Maybe String
applyMods' mods = fmap (`applyMods` mods)
applyMods :: String -> [TokenModificator] -> String
applyMods = foldl (flip modify)

modify ::  TokenModificator -> String -> String
modify Capital = map toUpper
modify Initial = (:".") . head
modify Italics = prefixSuffixWith "*"
    where prefixSuffixWith prsf s = concat [prsf, s, prsf]
