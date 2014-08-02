module FQuoter.Parser.ParserSpec (main, spec) where

import Text.ParserCombinators.Parsec.Error
import qualified Data.Map as Map
import Test.Hspec
import FQuoter.Quote
import FQuoter.Parser.Parser
import FQuoter.Parser.ParserTypes
import FQuoter.Parser.ParsingErrors


main :: IO()
main = hspec spec

{- Utility function for parsing. -}
parseInput' inp = case parseInput inp of
                    Right x -> x
                    Left n -> error (show n)

parseError inp = case parseInput inp of
                    Right _ -> error "Input did not fail !"
                    Left a -> messageString . last . errorMessages $ a

insert' = Insert . Right
insertA' = insert' . PAuthor
insertS' = insert' . PSource
insertQ' = insert' . PQuote

{- Author insertion checked values -}
insertCharlesDickens = insertA' (Author (Just "Charles") (Just "Dickens") Nothing)
insertHomer = insertA' (Author Nothing Nothing (Just "Homer"))
insertVirgil = insertA' (Author (Just "Vergilius Maro") (Just "Publius") (Just "Virgil"))
insertTolkien = insertA' (Author (Just "John Ronald Reuel") (Just "Tolkien") Nothing)

{- Source insertion checked values -}
tale2cities = "A tale of two cities"
noMetadata = Map.empty
metadata' = Map.fromList[("Published date", "1887")
                        ,("Editor", "Penguin")]
dickensSearchTerm = ParserSource tale2cities ["Dickens"] noMetadata
sourceMetadata = ParserSource tale2cities ["Dickens"] metadata'
sourceMultiauthor = ParserSource "All the President's Men" ["Bob Woodward", "Carl Bernstein"] noMetadata
insertDickensSearchTerm = insertS' dickensSearchTerm
insertSourceMetadatas = insertS' sourceMetadata
insertMultiauthorSource = insertS' sourceMultiauthor

{- Parsing strings and values for quotes -}
ttitle = "A tale of two cities"
tquote = "It was the best of time, it was the worst of time"
ttags = "((Classic, Incipit))"
tcomment = "Come on man, was it the worst or the best ? Make up your mind !"
tauthor = "Dickens"

insertIncipit1 = "insert quote \"" ++ tquote ++ "\" in " ++ ttitle
insertIncipit2 =  insertIncipit1 ++ " at page 2"
insertIncipit3 = insertIncipit1 ++ " " ++ ttags
insertIncipit4 = insertIncipit1 ++ " [[" ++ tcomment ++ "]]"
insertIncipit5 = insertIncipit1 ++ " by Dickens"

testcase1_quote = "In ways that need not be doctrinal, strong poems are always omens of resurrection."
testcase1 = "insert quote \"" ++ testcase1_quote ++ "\" in anxiety at page xxiv"

correctIncipit1 = insertQ' $ ParserQuote tquote ttitle Nothing [] [] Nothing
correctIncipit2 = insertQ' $ ParserQuote tquote ttitle (Just "page 2") [] [] Nothing
correctIncipit3 = insertQ' $ ParserQuote tquote ttitle Nothing ["Classic", "Incipit"] [] Nothing
correctIncipit4 = insertQ' $ ParserQuote tquote ttitle Nothing [] [] (Just tcomment)
correctIncipit5 = insertQ' $ ParserQuote tquote ttitle Nothing [] ["Dickens"] Nothing

correcttestcase1 = insertQ' $ ParserQuote testcase1_quote "anxiety" (Just "page xxiv") [] [] Nothing

{- Search values -}
findTime = FindWord "time"
findClassicIncipit = FindTags ["Classic", "Incipit"]

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
    describe "Checks quote insertion commands" $ do
        it ("Parse basic quote") $ do
            parseInput' insertIncipit1 `shouldBe` correctIncipit1
        it ("Parse quote with localization" ++ insertIncipit2) $ do
            parseInput' insertIncipit2 `shouldBe` correctIncipit2
        it ("Parse quote with tags" ++ insertIncipit3) $ do
            parseInput' insertIncipit3 `shouldBe` correctIncipit3
        it ("Parse quote with comment" ++ insertIncipit4) $ do
            parseInput' insertIncipit4 `shouldBe` correctIncipit4
        it ("Parse quote with a title starting by a keyword" ++ testcase1) $ do
            parseInput' testcase1 `shouldBe` correcttestcase1
    describe "Checks search commands" $ do
        it ("Parse a search by word(s) command.") $ do
            parseInput' "search time" `shouldBe` findTime
        it ("Parse a search by tags command.") $ do
            parseInput' "search [Classic, Incipit]" `shouldBe` findClassicIncipit
    describe "Errors should get proper messages." $ do
        context "If information is missing..." $ do
            it "Explain expected input with no commands." $ do
                parseError "" `shouldBe` errorNoParsing
            it "Explain expected type with just insert." $ do
                parseError "insert" `shouldBe` errorInsertNoType
            it "Explain expected parameters with just search." $ do
                parseError "search" `shouldBe` errorSearchNoParam
