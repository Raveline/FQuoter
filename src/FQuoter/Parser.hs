module FQuoter.Parser 
(
    QuoteType (..),
    Action (..),
    parseInput
)
where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, (<|>))

import FQuoter.Quote

data QuoteType
    = TAuthor Author
    -- | TSource Source
    -- | TQuote Quote
    deriving (Eq, Show)

data Action 
    = Insert QuoteType
    deriving (Eq, Show)

{- A command contains a term (Insert, Search, Delete) and parameters. -}
-- command :: GenParser Char st Action
command = choice [ insert
                 -- , Insert <$> p_source
                 -- and others
                 ]

word = do w <- many1 alphaNum
          spaces
          if w == "aka" then fail "Reserved word" else return w

words' = word `manyTill` endWordChain

endWordChain = (lookAhead (char '\"') >> return ())
               <|> (lookAhead $ string "aka" >> return ())
               <|> eof

specifically s = do w <- string s
                    spaces
                    return w

insert  = do
            specifically "insert"
            author <- p_author
            return $ Insert author
lookup = undefined
delete = undefined

p_source = undefined -- TTCT

-- Author parsing
p_author = do
            specifically "author"
            auth <- try authorFullNameAndNick <|> authorFullNameOrNick
            return $ TAuthor auth

authorFromStringArray :: [String] -> Author
authorFromStringArray s
    | length s == 1 = Author Nothing Nothing (Just $ head s)
    | otherwise = Author (Just $ unwords . init $ s) (Just $ last s) Nothing

authorFullNameOrNick = do name <- words'
                          return $ authorFromStringArray name

authorFullNameAndNick = do name <- words'                      
                           nickName <- akaPseudonym <|> bracketPseudonym
                           return $ Author (Just $ unwords . init $ name) (Just $ last name) (Just nickName)

akaPseudonym = do
                string "aka"
                spaces
                nickName <- word
                return nickName

bracketPseudonym = between (char '"') (char '"') word

parseInput :: String -> Either ParseError Action
parseInput = parse command "(unknown)" 
