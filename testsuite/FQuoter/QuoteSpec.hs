module FQuoter.QuoteSpec (main, spec) where

import Test.Hspec
import FQuoter.Quote

henryJames = Author (Just "Henry") (Just "James") Nothing
homer = Author Nothing Nothing (Just "Homer")
anonymousAuthor = Author Nothing Nothing Nothing 

main :: IO()
main = hspec spec

spec = do
    describe "Check author display." $ do
        it "Displays the full name of a given author with no surname" $ do
            show henryJames `shouldBe` "Henry James"
        it "Displays the surname of a given author with no name but a surname" $ do
            show homer `shouldBe` "Homer"
        it "Displays anonymous for an author with no value." $ do
            show anonymousAuthor `shouldBe` anonymous
