module FQuoter.Serialize.GroupingSpec(main, spec) where

import Test.Hspec

import Database.HDBC
import FQuoter.Serialize.Grouping
import FQuoter.Serialize.SerializedTypes

-------------
-- Test data 
-------------
main :: IO()
main = hspec spec

single' = Single . toSql

testData = [["1","page 3", "quote", "", "title", "publisher", "random", "tag", "John", "Doe", ""]
           ,["1","page 3", "quote", "", "title", "publisher", "random", "tag1", "John", "Doe", ""]
           ,["1","page 3", "quote", "", "title", "publisher", "random", "tag", "Jane", "Doe", ""]
           ,["1","page 3", "quote", "", "title", "publisher", "random", "tag1", "Jane", "Doe", ""]
           ,["2","page 42", "another quote", "comment", "title2", "", "", "", "Bill", "Doe", ""]]

exempleInput = map (map toSql) testData

expectedOutut = 
    [[Grouped [Grouped [single' "publisher", single' "random"]]
     ,Grouped [single' "tag", single' "tag1"]
     ,Grouped [Grouped [single' "John", single' "Doe", single' ""]
              ,Grouped [single' "Jane", single' "Doe", single' ""]]
     ,single' "1", single' "page 3", single' "quote", single' "", single' "title"]
    ,[Grouped [Grouped [single' "", single' ""]]
     ,Grouped [single' ""]
     ,Grouped [Grouped [single' "Bill", single' "Doe", single' ""]]
     ,single' "2", single' "page 42", single' "another quote", single' "comment"
     ,single' "title2"]]


spec = do
        describe "Gropuping" $ do
            context "Grouping a list of 4 results containing 2 quotes, one having 2 authors" $ do
                it "Return a table of length 2" $ do
                    groupSql DBQuote exempleInput `shouldSatisfy` ((==2) . length)
                it "Return a array with grouped metadatas, tags, authors" $ do
                    groupSql DBQuote exempleInput `shouldBe` expectedOutut
