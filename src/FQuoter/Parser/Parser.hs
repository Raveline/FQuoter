module FQuoter.Parser.Parser 
(
    Action (..),
    ParserSource (..),
    parseInput
)
where

import Control.Monad
import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, (<|>))

import FQuoter.Quote hiding (string)
import FQuoter.Parser.ParserTypes

import qualified Data.Map as Map

data Action 
    = Insert ParsedType
    deriving (Eq, Show)

{- A command contains a term (Insert, Search, Delete) and parameters. -}
-- command :: GenParser Char st Action
command = choice [ insert
                 -- , Insert <$> pSource
                 -- and others
                 ]

word = do w <- many1 (alphaNum <|> char '\'')
          spaces
          if w == "aka" then fail "Reserved word" else return w

words' = spaces >> word `manyTill` endWordChain

simpleString = do
                w <- words' 
                return $ unwords w
                  

endWordChain = void (lookAhead symbols)
               <|> void (lookAhead keywords)
               <|> eof

keywords = string "aka"
          <|> string "by"

symbols = oneOf "(){}\":,"

betweenBrackets = between (char '"') (char '"') simpleString
                    

specifically s = do w <- string s
                    spaces
                    return w

insert  = specifically "insert" >> choice [Insert <$> pAuthor
                                          ,Insert <$> pSource]

lookup = undefined
delete = undefined

-- Author parsing
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
                           nickName <- akaPseudonym <|> betweenBrackets
                           return $ Author (Just $ unwords . init $ name) (Just $ last name) (Just nickName)

akaPseudonym = do
                string "aka"
                spaces
                simpleString

-- Source parsing
pSource = do string "source"
             spaces
             title <- betweenBrackets
             spaces
             string "by"
             spaces
             authors <- (betweenBrackets <|> simpleString) `sepBy` (char ',' <* spaces)
             metadata <- option Map.empty parseMetadata
             return $ PSource $ ParserSource title authors metadata

parseMetadata = do
                    elems <- between (char '{' <* spaces)(char '}' <* spaces) variousValues
                    return $ Map.fromList elems

variousValues = valuePair `sepBy` (char ',' <* spaces)

valuePair = do
                info <- simpleString
                char ':' 
                value <- simpleString
                return (info, value)

parseInput :: String -> Either ParseError Action
parseInput = parse command "(unknown)" 
