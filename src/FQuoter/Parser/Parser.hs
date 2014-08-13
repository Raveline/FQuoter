module FQuoter.Parser.Parser 
(
    Action (..),
    parseInput
)
where

import Control.Monad
import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, (<|>))

import FQuoter.Quote hiding (string, Quote(..), content)
import FQuoter.Parser.ParserTypes
import FQuoter.Parser.ParsingErrors
import FQuoter.Serialize.SerializedTypes

import qualified Data.Map as Map

-----------
-- Exposed
-----------

-- Result of parsing.
data Action 
    = Insert (Either NotDefinedType ParsedType)
    | FindWord String
    | FindTags [String]
    | Remove DBType String
    deriving (Eq, Show)

parseInput :: String -> Either ParseError Action
parseInput = parse command "(unknown)" 

{- A command contains a term (Insert, Search, Delete) and parameters. -}
-- command :: GenParser Char st Action
command = choice [ insert
                 , find
                 , delete
                 -- and others
                 ] <?> errorNoParsing

--------------
-- Utilities
--------------
-- Shamelessly stolen from Christoph Schiessl's blog
many1Till p end = do
    notFollowedBy end
    first <- p
    rest <- manyTill p end
    return (first:rest)

authorsList = (specifically "by" *> (betweenQuotes <|> simpleString) `sepBy` (char ',' <* spaces))
{- Get a simple word made of letters and/or numbers -}
word = many1 (alphaNum <|> char '\'') <* spaces

{- Read words till a special character or a word chaine is found -}
words' = spaces >> word `many1Till` endWordChain
    where
        endWordChain = void (lookAhead symbols)
                    <|> void (lookAhead keywords)
                    <|> eof
        keywords = try (string "at ") 
                   <|> try (string "aka ")
                   <|> try (string "by ")
                   <|> try (string "in ")
        symbols = oneOf "(){}[]\":,"

{- Return a list of words as a single string. -}
simpleString = unwords <$> words'

{- Read a string between double quotes -}
betweenQuotes = between (char '"') (char '"') (many $ noneOf "\"") 

{- Read words between brackets. -}
betweenBrackets = between (char '[') (char ']') (simpleString `sepBy` (char ',' <* spaces))

{- Parse a defined word and the spaces after it -}
specifically s = string s <* spaces

{- Parse key values separated by columns and commas. -}
variousValues = valuePair `sepBy` (char ',' <* spaces)
    where
        valuePair = (,) <$> simpleString <*> (char ':' *> simpleString)

--------------
-- Insertion
--------------
insert  = specifically "insert" >> choice [Insert <$> pAuthor
                                          ,Insert <$> pSource
                                          ,Insert <$> pQuote
                                          ,fail errorInsertNoType]

find = (specifically "find" <|> specifically "search")
       >> choice [FindTags <$> betweenBrackets
                 ,FindWord <$> simpleString
                 ,fail errorSearchNoParam]

-----------------
-- Author parsing
-----------------
pAuthor = specifically "author"
          *>  option (Left NDAuthor) 
          (Right <$> (PAuthor <$> (try authorFullNameAndNick 
                                  <|> authorFullNameOrNick)))

authorFromStringArray :: [String] -> Author
authorFromStringArray s
    | length s == 1 = Author Nothing Nothing (Just $ head s)
    | otherwise = Author (Just $ unwords . init $ s) (Just $ last s) Nothing

authorFullNameOrNick = do name <- words'
                          return $ authorFromStringArray name

authorFullNameAndNick = do name <- words'                      
                           nickName <- akaPseudonym <|> betweenQuotes
                           return $ Author (Just $ unwords . init $ name) (Just $ last name) (Just nickName)

akaPseudonym = specifically "aka" >> simpleString

-----------------
-- Source parsing
-----------------
pSource = specifically "source"
          *> option (Left NDSource) (Right <$>
                            (PSource <$>
                            (ParserSource 
                            <$> title
                            <*> authorsList
                            <*> meta)))
          where
            title = betweenQuotes <* spaces
            meta = option Map.empty parseMetadata

parseMetadata = Map.fromList <$> between (char '{' <* spaces)(char '}' <* spaces) variousValues

----------------
-- Quote parsing
----------------
pQuote = specifically "quote" <* spaces
         *> option (Left NDQuote) pQuote'
            where
                pQuote' = Right <$>
                            (PQuote <$> 
                            (ParserQuote 
                            <$> content 
                            <*> source 
                            <*> loc 
                            <*> tags 
                            <*> authors 
                            <*> comment))
                content = betweenQuotes <* spaces
                source = (specifically "in" *> (simpleString <|> betweenQuotes))
                authors = option [] authorsList
                tags = option [] (pTags <* spaces)
                loc = option Nothing (pLocation <* spaces) 
                comment = option Nothing (pComment <* spaces)

pLocation = Just <$> (specifically "at" *> simpleString)

pTags = between (string "((" <* spaces) (string "))" <* spaces) (simpleString `sepBy` (char ','))

pComment = Just <$> between (string "[[" <* spaces) (string "]]" <* spaces) (many $ noneOf "]")

------------
-- Deletion
------------

delete = specifically "delete"  *> (Remove <$> pickDeleteType <*> (spaces *> many alphaNum))
    where 
        pickDeleteType :: GenParser Char st DBType
        pickDeleteType = choice [string "author" *> return DBAuthor
                                ,string "source" *> return DBSource
                                ,string "quote" *> return DBQuote]
