module FQuoter.Parser.ParserSpec (main, spec) where

import qualified Data.Map as Map
import Test.Hspec
import FQuoter.Quote
import FQuoter.Parser.Parser
import FQuoter.Parser.ParserTypes


main :: IO()
main = hspec spec

insertCharlesDickens = Insert $ PAuthor (Author (Just "Charles") (Just "Dickens") Nothing)
insertHomer = Insert $ PAuthor (Author Nothing Nothing (Just "Homer"))
insertVirgil = Insert $ PAuthor (Author (Just "Vergilius Maro") (Just "Publius") (Just "Virgil"))
insertTolkien = Insert $ PAuthor (Author (Just "John Ronald Reuel") (Just "Tolkien") Nothing)

tale2cities = "A tale of two cities"

noMetadata = Map.empty
metadata' = Map.fromList[("Published date", "1887")
                        ,("Editor", "Penguin")]

dickensSearchTerm = ParserSource tale2cities ["Dickens"] noMetadata
sourceMetadata = ParserSource tale2cities ["Dickens"] metadata'
sourceMultiauthor = ParserSource "All the President's Men" ["Bob Woodward", "Carl Bernstein"] noMetadata

insertDickensSearchTerm = Insert $ PSource dickensSearchTerm
insertSourceMetadatas = Insert $ PSource sourceMetadata
insertMultiauthorSource = Insert $ PSource sourceMultiauthor

parseInput' inp = case parseInput inp of
                    Right x -> x
                    Left n -> error (show n)

-- must pass :
-- insert quote "It was the best of time, it was the worst of time" in A tale of two cities page 2
-- insert quote "It was the best of time, it was the worst of time" in A tale of two cities page 2 ((Classic, Incipit))
spec = do
    describe "Check author insertion commands." $ do
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
    describe "Checks sources insertion commands" $ do
        it "Parse a basic source with no metadata and an author search-term" $ do
            parseInput' "insert source \"A tale of two cities\" by Dickens" `shouldBe` insertDickensSearchTerm
        it "Parse a source with two authors" $ do
            parseInput' "insert source \"All the President's Men\" by Bob Woodward, Carl Bernstein" `shouldBe` insertMultiauthorSource
        it "Parse a basic source with metadatas and an author search-term" $ do
            parseInput' "insert source \"A tale of two cities\" by Dickens { Published date : 1887, Editor : Penguin }" `shouldBe` insertSourceMetadatas
        -- TODO : make sure the anonymous author is recognzied with the all Nothing author
        -- TODO : make sure a title with a comma can pass
