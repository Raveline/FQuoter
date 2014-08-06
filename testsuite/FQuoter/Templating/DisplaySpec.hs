module FQuoter.Templating.DisplaySpec (main, spec) where

import Text.ParserCombinators.Parsec.Error
import qualified Data.Map as Map
import Test.Hspec
import FQuoter.Quote
import FQuoter.Templating.Display


main :: IO()
main = hspec spec

defaultTree =   [Every 
                    (Or [One (Token [Capital] $ AuthorInfo AuthorLastName)
                        ,One (Token [] $ ConstantString ", ")
                        ,One (Token [Initial, Capital] $ AuthorInfo AuthorFirstName)]
                        [One (Token [Capital] $ AuthorInfo AuthorNickName)])
                , One (Token [] $ ConstantString " (")
                 ,One (Token [] (SourceInfo $ SourceMetadata "Date"))
                 ,One (Token [] $ ConstantString ") ")
                 ,One (Token [Italics] $ (SourceInfo SourceTitle))
                 ,One (Token [] $ ConstantString ". ")
                 ,One (Token [] (SourceInfo $ SourceMetadata "Place"))
                 ,One (Token [] $ ConstantString " : ")
                 ,One (Token [] (SourceInfo $ SourceMetadata "Publisher"))
                 ,One (Token [] $ ConstantString ".")]

constantCaps = [One $ Token [Capital] (ConstantString "test")]
constantInit = [One $ Token [Initial] (ConstantString "test")]
constantCapsInit = [One $ Token [Capital, Initial] (ConstantString "test")]

mtd :: (String, String) -> (MetadataInfo, MetadataValue)
mtd (a,b) = (MetadataInfo . QuoterString $ a, MetadataValue . QuoterString $ b)

henryJames = Author (Just "Henry") (Just "James") (Just "")
turnOfTheScrew = Source "The turn of a screw" [henryJames] 
                        (Map.fromList [mtd("Date", "1898")
                                     ,mtd("Place", "London")
                                     ,mtd("Publisher", "William Heinemann")])
q1 = Quote [henryJames] turnOfTheScrew "" (Just "p.3") [] Nothing

defaultDisplay = "JAMES, H. (1898) *The turn of a screw*. London : William Heinemann."

spec = do
    describe "String should be modified according to Modificators" $ do
        it "Apply capital modificator to write a word in full caps" $ do
            readTree constantCaps q1 `shouldBe` "TEST"
        it "Apply initial modificator to write only the first letter of a word" $ do
            readTree constantInit q1 `shouldBe` "t"
        it "Apply capital and initial to write both letters of a word" $ do
            readTree constantCapsInit q1 `shouldBe` "T"      
    describe "Full quotes are to be displayed following the rules of the template." $ do
        it "Apply the default tree properly" $ do
            readTree defaultTree q1 `shouldBe` defaultDisplay
