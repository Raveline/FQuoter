module FQuoter.ParserSpec (main, spec) where

import Test.Hspec
import FQuoter.Quote
import FQuoter.Parser


main :: IO()
main = hspec spec

insertCharlesDickens = Insert $ TAuthor (Author (Just "Charles") (Just "Dickens") Nothing)
insertHomer = Insert $ TAuthor (Author Nothing Nothing (Just "Homer"))
insertVirgil = Insert $ TAuthor (Author (Just "Vergilius Maro") (Just "Publius") (Just "Virgil"))
insertTolkien = Insert $ TAuthor (Author (Just "John Ronald Reuel") (Just "Tolkien") Nothing)

parseInput' inp = case parseInput inp of
                    Right x -> x
                    Left n -> error (show n)

-- must pass :
-- insert author "Charles Dickens"
-- insert source "A tale of two cities" by "Charles Dickens"
-- insert source "A tale of two cities" by Dickens
-- insert source "A tale of two cities" by Dickens { Published date : 1887, Editor : Penguin }
-- insert quote "It was the best of time, it was the worst of time" in A tale of two cities page 2
-- insert quote "It was the best of time, it was the worst of time" in A tale of two cities page 2 ((Classic, Incipit))
spec = do
    describe "Check insertion commands." $ do
        it "Parse a simple author insertion" $ do
            parseInput' "insert author Charles Dickens" `shouldBe` insertCharlesDickens
        it "Parse a insert author with a nickname" $ do
            parseInput' "insert author Homer" `shouldBe` insertHomer
        it "Parses a insert author with a multiple first name" $ do
            parseInput' "insert author John Ronald Reuel Tolkien" `shouldBe` insertTolkien
        it "Parses a insert author with a nickname in brackets" $ do
            parseInput' "insert author Vergilius Maro Publius \"Virgil\"" `shouldBe` insertVirgil
        it "Parse a insert author with a double first name and a nickname" $ do
            parseInput' "insert author Vergilius Maro Publius aka Virgil" `shouldBe` insertVirgil
