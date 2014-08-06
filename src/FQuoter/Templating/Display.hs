module FQuoter.Templating.Display
where

import FQuoter.Quote
import Data.Maybe
import Data.Monoid
import Data.Char
import qualified Data.Map as Map

data Token =
    Token [TokenModificator] TokenContent

data TokenNode =
    One Token
    | UpTo Int TokenNode
    | Every TokenNode
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

displayTree = [Token]

defaultTemplate = "currentDisplay:"
    ++ "[Author-> {maj}%al, {maj,init}%af|{maj}%an]"
    ++ "(%metaDate) {it}%t. %metaPlace:%metaPublisher"

readTree :: [TokenNode] -> Quote -> String
readTree q s = fromMaybe "" $ mapSeveralNodes readDisplayNode q s

readDisplayNode :: TokenNode -> Quote -> Maybe String
readDisplayNode (Every ts) q
    | typeForOne ts == Authorship = mapSeveralItems treeForAuthors ts . author $ q
    | otherwise                   = error "Every only works for authors for now."
readDisplayNode (UpTo n ts) q 
    | typeForOne ts == Authorship =  mapSeveralItems treeForAuthors ts . take n . author $ q
    | otherwise                   = error "UpTo only works for authors for now."
readDisplayNode t@(One (Token _ (SourceInfo _))) q = treeForSource t . source $ q
readDisplayNode t@(One (Token _ (AuthorInfo _))) q = treeForAuthors t . head . author $ q
readDisplayNode (One (Token mods (ConstantString s))) _ = Just . applyMods s $ mods

treeForAuthors :: TokenNode -> Author -> Maybe String
treeForAuthors (One (Token mods (AuthorInfo ai))) s = applyMods' mods . readAuthorToken ai $ s
    where
        readAuthorToken (AuthorNickName)    = surname
        readAuthorToken (AuthorLastName)    = lastName
        readAuthorToken (AuthorFirstName)   = firstName
treeForAuthors (One (Token mods (ConstantString s))) _ = Just . applyMods s $ mods
treeForAuthors (Or t1 t2) s = orNode treeForAuthors t1 t2 $ s
treeForAuthors _ _ = error "Only One and Or allowed"

treeForSource :: TokenNode -> Source -> Maybe String
treeForSource (One (Token mods (SourceInfo si))) s = applyMods' mods . readSourceToken si $ s
    where
        readSourceToken (SourceTitle)         = Just . title
        readSourceToken (SourceMetadata s)    = lookupMetadata s
treeForSource (One (Token mods (ConstantString s))) _ = Just . applyMods s $ mods
treeForSource (Or t1 t2) s =  orNode treeForSource t1 t2 $ s
treeForSource _ _ = error "Only One and Or allowed"

mapSeveralNodes :: (TokenNode -> a -> Maybe String) -> [TokenNode] -> a -> Maybe String
mapSeveralNodes f t a = mconcat . map (flip f $ a) $ t

mapSeveralItems :: (TokenNode -> a -> Maybe String) -> TokenNode -> [a] -> Maybe String
mapSeveralItems f t as = mconcat . map (f t) $ as

orNode :: (TokenNode -> a -> Maybe String) -> [TokenNode] -> [TokenNode] -> a -> Maybe String
orNode f n1 n2 a
    | isNothing x = mapSeveralNodes f n2 a
    | otherwise = x
    where x = mapSeveralNodes f n1 a
    
typeForOne :: TokenNode -> TokenType
typeForOne (One (Token _ (SourceInfo _)))     = Sourceship
typeForOne (One (Token _ (AuthorInfo _)))     = Authorship
typeForOne (One (Token _ (ConstantString _))) = Constant
typeForOne (Every tn)                   = typeForOne tn
typeForOne (UpTo _ tn)                  = typeForOne tn
typeForOne (Or tn _)                    = typeForOne . head $ tn

type DisplayTemplate = String
display :: DisplayTemplate -> Quote -> String
display t = undefined

applyMods' :: [TokenModificator] -> Maybe String -> Maybe String
applyMods' mods = fmap ((flip applyMods) mods)
applyMods :: String -> [TokenModificator] -> String
applyMods = foldl (flip modify)

modify ::  TokenModificator -> String -> String
modify Capital = map toUpper
modify Initial = return . head
modify Italics = prefixSuffixWith "*"
    where prefixSuffixWith elem s = concat [elem, s, elem]
