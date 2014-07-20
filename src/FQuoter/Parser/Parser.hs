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

import qualified Data.Map as Map

-----------
-- Exposed
-----------

-- Result of parsing.
data Action 
    = Insert ParsedType
    | FindWord String
    | FindTag [String]
    deriving (Eq, Show)

parseInput :: String -> Either ParseError Action
parseInput = parse command "(unknown)" 

{- A command contains a term (Insert, Search, Delete) and parameters. -}
-- command :: GenParser Char st Action
command = choice [ insert
                 , find
                 -- and others
                 ]

--------------
-- Utilities
--------------

{- Get a simple word made of letters and/or numbers -}
word = many1 (alphaNum <|> char '\'') <* spaces

{- Read words till a special character or a word chaine is found -}
words' = spaces >> word `manyTill` endWordChain
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
                                          ,Insert <$> pQuote]

find = (specifically "find" <|> specifically "search")
       >> choice [FindTag <$> betweenBrackets
                 ,FindWord <$> simpleString]

delete = undefined

-----------------
-- Author parsing
-----------------
pAuthor = do
            specifically "author"
            auth <- try authorFullNameAndNick <|> authorFullNameOrNick
            return $ PAuthor auth

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
pSource = PSource <$> pSource'
    where pSource' = ParserSource <$> title
                                  <*> authors
                                  <*> meta
          title = (specifically "source" *> (betweenQuotes <* spaces))
          authors = (specifically "by" *> (betweenQuotes <|> simpleString) `sepBy` (char ',' <* spaces))
          meta = option Map.empty parseMetadata

parseMetadata = Map.fromList <$> between (char '{' <* spaces)(char '}' <* spaces) variousValues

----------------
-- Quote parsing
----------------
pQuote = PQuote <$> pQuote'
    where pQuote' = ParserQuote <$> content
                                <*> source
                                <*> loc
                                <*> tags
                                <*> comment
          content = (specifically "quote" *> (betweenQuotes <* spaces))
          source = (specifically "in" *> (simpleString <|> betweenQuotes))
          tags = option [] (pTags <* spaces)
          loc = option Nothing (pLocation <* spaces) 
          comment = option Nothing (pComment <* spaces)

pLocation = Just <$> (specifically "at" *> simpleString)

pTags = between (string "((" <* spaces) (string "))" <* spaces) (simpleString `sepBy` (char ','))

pComment = Just <$> between (string "[[" <* spaces) (string "]]" <* spaces) (many $ noneOf "]")

