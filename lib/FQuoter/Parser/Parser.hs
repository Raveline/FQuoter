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
import FQuoter.Actions
import FQuoter.Parser.ParserTypes
import FQuoter.Parser.ParsingErrors
import FQuoter.Serialize.SerializedTypes

import qualified Data.Map as Map

-----------
-- Exposed
-----------
parseInput :: String -> Either ParseError Action
parseInput = parse command "(unknown)" 

{- A command contains a term (Insert, Search, Delete) and parameters. -}
-- command :: GenParser Char st Action
command :: GenParser Char st Action
command = choice [ insert
                 , try find
                 , delete
                 , update
                 , try shell
                 -- and others
                 ] <?> errorNoParsing

--------------
-- Utilities
--------------
many1Till :: GenParser t st a -> GenParser t st end -> GenParser t st [a]
many1Till p end = liftM2 (:) p (manyTill p end)

authorsList :: GenParser Char st [String]
authorsList = specifically "by" *> (betweenQuotes <|> simpleString) `sepBy` (char ',' <* spaces)

-- Get a simple word made of letters and/or numbers
word :: GenParser Char st String
word = many1 (alphaNum <|> char '\'') <* spaces

-- Read words till a special character or a word chaine is found
words' :: GenParser Char st [String]
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
simpleString :: GenParser Char st String
simpleString = unwords <$> words'

{- Read a string between double quotes -}
betweenQuotes :: GenParser Char st String
betweenQuotes = between (char '"') (char '"') (many $ noneOf "\"") 

{- Read words between brackets. -}
betweenBrackets :: GenParser Char st [String]
betweenBrackets = between (char '[') (char ']') (simpleString `sepBy` (char ',' <* spaces))

{- Parse a defined word and the spaces after it -}
specifically :: String -> GenParser Char st String
specifically s = string s <* spaces

{- Parse key values separated by columns and commas. -}
variousValues :: GenParser Char st [(String, String)]
variousValues = valuePair `sepBy` (char ',' <* spaces)
    where
        valuePair = (,) <$> simpleString <*> (char ':' *> simpleString)

--------------
-- Insertion
--------------
insert :: GenParser Char st Action
insert  = specifically "insert" >> choice [Insert <$> pAuthor
                                          ,Insert <$> pSource
                                          ,Insert <$> pQuote
                                          ,fail errorInsertNoType]

find :: GenParser Char st Action
find = (specifically "find" <|> specifically "search")
       >> choice [FindTags <$> betweenBrackets
                 ,FindWord <$> simpleString
                 ,fail errorSearchNoParam]

-----------------
-- Author parsing
-----------------
pAuthor :: GenParser Char st (Either NotDefinedType ParsedType)
pAuthor = specifically "author"
          *>  option (Left NDAuthor) 
          (Right <$> (PAuthor <$> (try authorFullNameAndNick 
                                  <|> authorFullNameOrNick)))

authorFromStringArray :: [String] -> Author
authorFromStringArray s
    | length s == 1 = Author Nothing Nothing (Just $ head s)
    | otherwise = Author (Just $ unwords . init $ s) (Just $ last s) Nothing

authorFullNameOrNick :: GenParser Char st Author
authorFullNameOrNick = do name <- words'
                          return $ authorFromStringArray name

authorFullNameAndNick :: GenParser Char st Author
authorFullNameAndNick = do name <- words'
                           nickName <- akaPseudonym <|> betweenQuotes
                           return $ Author (Just $ unwords . init $ name) (Just $ last name) (Just nickName)

akaPseudonym :: GenParser Char st String
akaPseudonym = specifically "aka" >> simpleString

-----------------
-- Source parsing
-----------------
pSource :: GenParser Char st (Either NotDefinedType ParsedType)
pSource = specifically "source"
          *> option (Left NDSource) (Right <$>
                            (PSource <$>
                            (ParserSource 
                            <$> title'
                            <*> authorsList
                            <*> meta)))
          where
            title' = betweenQuotes <* spaces
            meta = option Map.empty parseMetadata

parseMetadata :: GenParser Char st (Map.Map String String)
parseMetadata = Map.fromList <$> between (char '{' <* spaces)(char '}' <* spaces) variousValues

----------------
-- Quote parsing
----------------
pQuote :: GenParser Char st (Either NotDefinedType ParsedType)
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
                            <*> authors'
                            <*> comment))
                content = betweenQuotes <* spaces
                source = specifically "in" *> (simpleString <|> betweenQuotes)
                authors' = option [] authorsList
                tags = option [] (pTags <* spaces)
                loc = option Nothing (pLocation <* spaces) 
                comment = option Nothing (pComment <* spaces)

pLocation :: GenParser Char st (Maybe String)
pLocation = Just <$> (specifically "at" *> simpleString)

pTags :: GenParser Char st [String]
pTags = between (string "((" <* spaces) (string "))" <* spaces) (simpleString `sepBy` char ',')

pComment :: GenParser Char st (Maybe String)
pComment = Just <$> between (string "[[" <* spaces) (string "]]" <* spaces) (many $ noneOf "]")

------------
-- Deletion
------------

delete :: GenParser Char st Action
delete = specifically "delete"  *> (Remove <$> pickType <*> (spaces *> many alphaNum))

pickType :: GenParser Char st DBType
pickType = choice [string "author" *> return DBAuthor
                  ,string "source" *> return DBSource
                  ,string "quote" *> return DBQuote]

-------------
-- Updating
------------

update :: GenParser Char st Action
update = specifically "update" *> (Updating <$> (spaces *> pickType)
                                          <*> (spaces *> many alphaNum)
                                          <*> (spaces *> pickUpdate)
                                          <*> (spaces *> pickProperty))

pickUpdate :: GenParser Char st Update
pickUpdate = choice [string "add" *> return Add
                    ,string "set" *> return Set
                    ,string "remove" *> return Delete]

pickProperty :: GenParser Char st TypeProperty
pickProperty = choice [authorProperties, sourceProperties]

authorProperties :: GenParser Char st TypeProperty
authorProperties = ModifyAuthor <$> choice 
                    [(string "first name" *> return AuthorFirstName) <*> readOrNothing
                    ,(string "last name" *> return AuthorLastName) <*> readOrNothing
                    ,(string "nickname" *> return AuthorNickName) <*> readOrNothing]

readOrNothing :: GenParser Char st (Maybe String)
readOrNothing = spaces *> (Just <$> many anyChar) <|> return Nothing

sourceProperties :: GenParser Char st TypeProperty
sourceProperties = 
    ModifySource <$> 
        choice [(string "title" *> return SourceTitle) 
                    <*> (spaces *> many anyChar)
               ,string "metadata" *> (SourceMetadata 
                    <$> (spaces *> many (noneOf ","))
                    <*> ( Just <$> (char ',' *> spaces *> many anyChar)
                               <|> return Nothing ))
               ,(string "author" *> return SourceAuthors)
                    <*> (spaces *> many alphaNum <* spaces) `sepBy` char ',']

-----------
-- Shell
-----------

shell :: GenParser Char st Action
shell = string "shell" *> return Shell
