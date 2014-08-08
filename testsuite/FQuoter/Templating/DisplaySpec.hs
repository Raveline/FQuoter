module FQuoter.Templating.DisplaySpec (main, spec) where

import Text.ParserCombinators.Parsec.Error
import qualified Data.Map as Map
import Test.Hspec

import FQuoter.Quote
import FQuoter.Templating.Display
import FQuoter.Templating.TemplateTypes


main :: IO()
main = hspec spec

defaultTree =   [SomeAuthors All
                    [Or [One [Capital] $ AuthorInfo AuthorLastName
                        ,One [] (ConstantString ", ")
                        ,One [Initial, Capital] $ AuthorInfo AuthorFirstName]
                        [One [Capital] $ AuthorInfo AuthorNickName]]
                , One [] (ConstantString " ")
                , Condition (SourceInfo $ SourceMetadata "Date")
                    [One [] $ ConstantString "("
                    ,One [] (SourceInfo $ SourceMetadata "Date")
                    ,One [] $ ConstantString ") "]
                 ,One [Italics] $ (SourceInfo SourceTitle)
                 ,One [] $ ConstantString ". "
                 ,One [] (SourceInfo $ SourceMetadata "Place")
                 ,One [] $ ConstantString " : "
                 ,One [] (SourceInfo $ SourceMetadata "Publisher")
                 ,One [] $ ConstantString "."]

constantCaps = [One [Capital] (ConstantString "test")]
constantInit = [One [Initial] (ConstantString "test")]
constantCapsInit = [One [Capital, Initial] (ConstantString "test")]

mtd :: (String, String) -> (MetadataInfo, MetadataValue)
mtd (a,b) = (MetadataInfo . QuoterString $ a, MetadataValue . QuoterString $ b)

henryJames = Author (Just "Henry") (Just "James") (Just "")
turnOfTheScrew = Source "The turn of a screw" [henryJames] 
                        (Map.fromList [mtd("Date", "1898")
                                     ,mtd("Place", "London")
                                     ,mtd("Publisher", "William Heinemann")])
incompleteTurnOfTheScrew = Source "The turn of a screw" [henryJames] 
                        (Map.fromList [mtd("Place", "London")
                                     ,mtd("Publisher", "William Heinemann")])
q1 = Quote [henryJames] turnOfTheScrew "" (Just "p.3") [] Nothing
q2 = Quote [henryJames] incompleteTurnOfTheScrew "" (Just "p.3") [] Nothing

defaultDisplay = "JAMES, H. (1898) *The turn of a screw*. London : William Heinemann."
defaultDisplay2 = "JAMES, H. *The turn of a screw*. London : William Heinemann."

spec = do
    describe "String should be modified according to Modificators" $ do
        it "Apply capital modificator to write a word in full caps" $ do
            readTree constantCaps q1 `shouldBe` "TEST"
        it "Apply initial modificator to write only the first letter of a word" $ do
            readTree constantInit q1 `shouldBe` "t."
        it "Apply capital and initial to write both letters of a word" $ do
            readTree constantCapsInit q1 `shouldBe` "T."
    describe "Full quotes are to be displayed following the rules of the template." $ do
        it "Apply the default tree properly" $ do
            readTree defaultTree q1 `shouldBe` defaultDisplay
        it "Apply the default tree properly, even with missing information" $ do
            readTree defaultTree q2 `shouldBe` defaultDisplay2
