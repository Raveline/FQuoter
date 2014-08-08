module FQuoter.Templating.TemplateParser
(parseTemplate)
where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import FQuoter.Templating.TemplateTypes

parseTemplate = parse (many parseToken) ""

parseToken :: GenParser Char st TokenNode
parseToken = parseMany <|> parseOr <|> parseOne 

parseOr :: GenParser Char st TokenNode
parseOr = char '|' *> (Or <$> tillPipe <*> tillPipe)
    where tillPipe = parseToken `manyTill` (char '|')

parseOne :: GenParser Char st TokenNode
parseOne = One <$> parseMods <*> ( (char '%' *> parseContent) <|> parseConstant)

parseMods :: GenParser Char st [TokenModificator]
parseMods = option [] bracketContent 
    where 
        bracketContent :: GenParser Char st [TokenModificator]
        bracketContent = between (char '{') (char '}') (parseMod `sepBy` (char ','))

parseMany :: GenParser Char st TokenNode
parseMany =  SomeAuthors <$> (( Only <$> nat ) <|> return All) 
                         <*> parseTokens 
    where
        parseTokens :: GenParser Char st [TokenNode]
        parseTokens = between (char '[') (char ']') (many parseToken)

parseContent :: GenParser Char st TokenContent
parseContent = choice [try $ AuthorInfo <$> (string "af" *> return AuthorFirstName)
                     ,try $ AuthorInfo <$> (string "al" *> return AuthorLastName)
                     ,try $ AuthorInfo <$> (string "an" *> return AuthorNickName)
                     ,SourceInfo <$> (string "t" *> return SourceTitle)
                     ,SourceInfo <$> (SourceMetadata <$> (string "meta" *> many letter))]

parseMod :: GenParser Char st TokenModificator
parseMod = choice [string "cap" *> return Capital
                  ,try $ string "init" *> return Initial
                  ,string "it" *> return Italics]

keyChar = "%{}[]|"

parseConstant :: GenParser Char st TokenContent
parseConstant = ConstantString <$> many1 (noneOf keyChar)
