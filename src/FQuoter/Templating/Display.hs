module FQuoter.Templating.Display
( readTree )
where

import Data.Maybe
import Control.Applicative
import Data.Monoid hiding (All)
import Data.Char
import qualified Data.Map as Map

import FQuoter.Quote
import FQuoter.Templating.TemplateTypes

readTree :: [TokenNode] -> Quote -> String
readTree q s = fromMaybe "" $ mapSeveralNodes evalNode q s

evalNode :: TokenNode -> Quote -> Maybe String
evalNode (One mods (SourceInfo SourceTitle)) = applyMods' mods . Just . title . source
evalNode (One mods (SourceInfo (SourceMetadata t))) = applyMods' mods . lookupMetadata t . source
evalNode t@(One mods (AuthorInfo _)) = evalAuthor t . mainAuthor
evalNode s@(SomeAuthors All ts) = mconcat . mapNodesAndItems evalAuthor ts . author
evalNode s@(SomeAuthors (Only n) ts) = mconcat . take n . mapNodesAndItems evalAuthor ts . author
evalNode (One mods (ConstantString s)) = displayConstantString mods s
evalNode (Condition con tn) = handleCondition con tn
evalNode (Or tn tn') = orNode evalNode tn tn'

handleCondition (SourceInfo (SourceMetadata m)) tn q
    | isNothing (lookupMetadata m $ source q) = Nothing
    | otherwise = mapSeveralNodes evalNode tn q
handleCondition (AuthorInfo ai) tn q
    = handleAuthorCondition ai tn $ mainAuthor q  

displayConstantString :: [TokenModificator] -> String -> a -> Maybe String
displayConstantString mods s _ = applyMods' mods . Just $ s

evalAuthor :: TokenNode -> Author -> Maybe String
evalAuthor (One mods (AuthorInfo ai)) = applyMods' mods . evalAuthorToken ai 
evalAuthor (One mods (ConstantString s)) = displayConstantString mods s
evalAuthor (Or tn tn') = orNode evalAuthor tn tn'
evalAuthor (Condition (AuthorInfo ai) tn) = handleAuthorCondition ai tn


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
    where prefixSuffixWith elem s = concat [elem, s, elem]
