module FQuoter.Templating.TemplateParserSpec (main, spec) where

import Text.ParserCombinators.Parsec.Error
import Test.Hspec

import FQuoter.Templating.TemplateTypes
import FQuoter.Templating.TemplateParser
import FQuoter.Config.Config

main :: IO()
main = hspec spec

parse f = case parseTemplate f of
            Left e -> error $ (show e)
            Right x -> x

simpleOne = "%al"
simpleOne' = [One [] $ AuthorInfo AuthorLastName]
simpleModOne = "{cap}%t"
simpleModOne' = [One [Capital] $ SourceInfo SourceTitle]
simpleOr = "|%al|%an|"
simpleOr' = [Or [One [] $ AuthorInfo AuthorLastName] [One [] $ AuthorInfo AuthorNickName]]
simpleMany = "[%al]"
simpleMany' = [SomeAuthors All [One [] $ AuthorInfo AuthorLastName]]
simpleConstant = "constant!"
simpleConstant' = [One [] $ ConstantString simpleConstant]

defaultTemplate = currentTemplate $ buildDefaultConfig

defaultTree =   [SomeAuthors (Only 2)
                    [Or [One [Capital] $ AuthorInfo AuthorLastName
                        ,One [] (ConstantString ", ")
                        ,One [Initial, Capital] $ AuthorInfo AuthorFirstName]
                        [One [Capital] $ AuthorInfo AuthorNickName]]
                 ,One [] $ ConstantString " "
                 ,Condition (SourceInfo $ SourceMetadata "Date")
                    [One [] $ ConstantString "("
                    ,One [] (SourceInfo $ SourceMetadata "Date")
                    ,One [] $ ConstantString ") "]
                 ,One [Italics] $ (SourceInfo SourceTitle)
                 ,One [] $ ConstantString ". "
                 ,One [] (SourceInfo $ SourceMetadata "Place")
                 ,One [] $ ConstantString " : "
                 ,One [] (SourceInfo $ SourceMetadata "Publisher")
                 ,One [] $ ConstantString "."]

spec = do
    describe "Template strings are transformed in Template Tree." $ do
        it "Read a simple template string with just one thing to display." $ do
            parse simpleOne `shouldBe` simpleOne'
        it "Read a template with just one thing to display, with modificator" $ do
            parse simpleModOne `shouldBe` simpleModOne'
        it "Read an Or case, with one thing to display or the other." $ do
            parse simpleOr `shouldBe` simpleOr'
        it "Read a multi-author display." $ do
            parse simpleMany `shouldBe` simpleMany'
        it "Read a constant." $ do
            parse simpleConstant `shouldBe` simpleConstant'
    describe "Default case can be handled." $ do
        it "Display gives the default parsing tree." $ do
            parse defaultTemplate `shouldBe` defaultTree
