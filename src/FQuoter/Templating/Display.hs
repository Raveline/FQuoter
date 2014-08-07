module FQuoter.Templating.Display
where

import Data.Maybe
import Control.Applicative
import Data.Monoid hiding (All)
import Data.Char
import qualified Data.Map as Map
import FQuoter.Quote

data Cardinality = All | Only Int

data TokenNode =
    One [TokenModificator] TokenContent
    | SomeAuthors Cardinality [TokenNode]
    | Or [TokenNode] [TokenNode]

data TokenContent =
    AuthorInfo TokenAuthorInfo
    | SourceInfo TokenSourceInfo
    | ConstantString String

data TokenSourceInfo =
    SourceTitle
    | SourceMetadata String

data TokenAuthorInfo =
    AuthorFirstName
    | AuthorLastName
    | AuthorNickName

data TokenModificator =
    Capital
    | Initial
    | Italics

data TokenType =
    Authorship
    | Sourceship
    | Constant
    deriving (Eq, Show)

defaultTemplate = "currentDisplay:"
    ++ "[{maj}%al, {maj,init}%af|{maj}%an]"
    ++ "(%metaDate) {it}%t. %metaPlace:%metaPublisher"

readTree :: [TokenNode] -> Quote -> String
readTree q s = fromMaybe "" $ mapSeveralNodes evalNode q s

evalNode :: TokenNode -> Quote -> Maybe String
evalNode (One mods (SourceInfo SourceTitle)) = applyMods' mods . Just . title . source
evalNode (One mods (SourceInfo (SourceMetadata t))) = applyMods' mods . lookupMetadata t . source
evalNode t@(One mods (AuthorInfo _)) = evalAuthor t . head . author
evalNode s@(SomeAuthors All ts) = mconcat . mapNodesAndItems evalAuthor ts . author
evalNode s@(SomeAuthors (Only n) ts) = mconcat . take n . mapNodesAndItems evalAuthor ts . author
evalNode (One mods (ConstantString s)) = displayConstantString mods s
evalNode (Or tn tn') = orNode evalNode tn tn'

displayConstantString :: [TokenModificator] -> String -> a -> Maybe String
displayConstantString mods s _ = applyMods' mods . Just $ s

evalAuthor :: TokenNode -> Author -> Maybe String
evalAuthor (One mods (AuthorInfo ai)) = applyMods' mods . readAuthorToken ai 
    where
        readAuthorToken (AuthorNickName)    = surname
        readAuthorToken (AuthorLastName)    = lastName
        readAuthorToken (AuthorFirstName)   = firstName
evalAuthor (One mods (ConstantString s)) = displayConstantString mods s
evalAuthor (Or tn tn') = orNode evalAuthor tn tn'

mapNodesAndItems :: (TokenNode -> a -> Maybe String) -> [TokenNode] -> [a] -> [Maybe String]
mapNodesAndItems f ts = map (mapSeveralNodes f ts)

mapSeveralNodes :: (TokenNode -> a -> Maybe String) -> [TokenNode] -> a -> Maybe String
mapSeveralNodes f t a = mconcat . map (flip f $ a) $ t

mapSeveralItems :: (TokenNode -> a -> Maybe String) -> TokenNode -> [a] -> Maybe String
mapSeveralItems f t = mconcat . map (f t)

orNode :: (TokenNode -> a -> Maybe String) -> [TokenNode] -> [TokenNode] -> a -> Maybe String
orNode f n1 n2 a = mapSeveralNodes f n1 a <|>  mapSeveralNodes f n2 a
    
type DisplayTemplate = String
display :: DisplayTemplate -> Quote -> String
display t = undefined

applyMods' :: [TokenModificator] -> Maybe String -> Maybe String
applyMods' mods = fmap ((flip applyMods) mods)
applyMods :: String -> [TokenModificator] -> String
applyMods = foldl (flip modify)

modify ::  TokenModificator -> String -> String
modify Capital = map toUpper
modify Initial = (:".") . head
modify Italics = prefixSuffixWith "*"
    where prefixSuffixWith elem s = concat [elem, s, elem]
