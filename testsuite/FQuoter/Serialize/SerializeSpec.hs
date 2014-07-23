module FQuoter.Serialize.SerializeSpec(main, spec) where

import Test.Hspec
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Free
import qualified Data.Map as Map

import FQuoter.Serialize.Serialize
import FQuoter.Serialize.Shortcuts
import FQuoter.Serialize.SerializedTypes
import FQuoter.Parser.ParserTypes
import FQuoter.Quote

-- todo
main :: IO()
main = hspec spec

type MockDB = Map.Map DBType (Map.Map String [DBValue SerializedType])

data InfoValue =
                -- For simple case. No need to simulate a DB 
                NoValue
                -- Simulating search in a DB. Keys are search terms.
                -- Values are result
               | ValueDictionary MockDB

tprocess :: (Monad m) => SerializationT (ReaderT InfoValue m) next -> ReaderT InfoValue m next
tprocess fr = runFreeT fr >>= tprocess'

tprocess'  :: (Monad m) => Serialization next (SerializationT (ReaderT InfoValue m) next) -> ReaderT InfoValue m next
tprocess' (Pure r) = return r
tprocess' (Free (Create _ n)) = tprocess n
tprocess' (Free (Search t (ByName s) n)) = do v <- ask
                                              res <- return $ retrieve t s v
                                              tprocess (n res)
tprocess' (Free (LastInsert n)) = tprocess (n 0)
tprocess' (Free (Associate _ _ n)) = tprocess n

retrieve :: DBType -> String -> InfoValue -> [DBValue SerializedType]
retrieve t s (ValueDictionary dic) = case Map.lookup t dic of
                                        Nothing -> []
                                        Just dic' -> Map.findWithDefault [] s dic'
retrive _ _ _ = error "Does not have a mock db, cannot retrieve. Test badly designed."

run' :: InfoValue -> FalliableSerialization InfoValue Identity a -> Either DBError a
run' inf f = runIdentity $ runExceptT $ runReaderT (tprocess f) inf


insertionSuccess = Right ()

-------------
-- Test data 
-------------
-- Authors
hJames = Author (Just "Henry") (Just "James") Nothing
hThoreau = Author (Just "Henry") (Just "Thoreau") Nothing
-- Sources
turnScrew = Source "The turn of the screw" [hJames] Map.empty
-- Quotes
tsqCont = "One wouldn’t flatter a child"
turnScrewQuote = Quote hJames turnScrew tsqCont Nothing ["aTag"] Nothing

----------------------- Parsed input
-------------- Authors : always ok
prsHjames = PAuthor $ hJames
------------- Sources
-- OK case
prsTurn = PSource $ ParserSource "The turn of the screw" ["James"] Map.empty
-- Non existing author
prsTurn' = PSource $ ParserSource "The turn of the screw" ["Jomes"] Map.empty
-- Ambiguous author
prsTurn'' = PSource $ ParserSource "The turn of the screw" ["Henry"] Map.empty
------------- Quotes
-- OK Case
prsdQuote = PQuote $ ParserQuote tsqCont "screw" (Just "Chapter 1") [] ["James"] Nothing
-- Non existing source
prsdQuote' = PQuote $ ParserQuote tsqCont "scraw" (Just "Chapter 1") [] ["James"] Nothing
-- Non existing author
prsdQuote'' = PQuote $ ParserQuote tsqCont "screw" (Just "Chapter 1") [] ["Jomes"] Nothing
------------- Search
-- No match
noMatch = "nomatch"
-- Matches
oneMatch = "child"

dict = Map.fromList [(DBAuthor
                    ,Map.fromList [("James", [DBValue 1 (SAuthor hJames)])
                                  ,("Henry", ambiguousHenry)])
                    ,(DBSource
                    ,Map.fromList [("screw", [DBValue 1 (SSource turnScrew)])])
                    ,(DBQuote
                    ,Map.fromList [("child", matchingChild)])
                    ]
mockDB = ValueDictionary dict
ambiguousHenry = [DBValue 1 (SAuthor hJames),DBValue 2 (SAuthor hThoreau)]
matchingChild = [DBValue 1 (SQuote turnScrewQuote)]

spec = do
        describe "Inserting" $ do
            context "When DB is coherent and input is not ambiguous" $ do
                it "Insert author normally." $ do
                    run' NoValue (insert prsHjames)  `shouldBe` insertionSuccess
                it "Insert a source normally." $ do
                    run' mockDB (insert prsTurn) `shouldBe` insertionSuccess
                it "Insert a quote normally." $ do
                    run' mockDB (insert prsdQuote) `shouldBe` insertionSuccess
            context "When DB is missing related authors, sources, etc." $ do
                it "Throw an error when tring to insert a source" $ do
                    run' mockDB (insert prsTurn') `shouldBe` Left (NonExistingDataError "Jomes")
                it "Throw an error when trying to insert a quote with missing source" $ do
                    run' mockDB (insert prsdQuote') `shouldBe` Left (NonExistingDataError "scraw")
                it "Throw an error when trying to insert a quote with missing author" $ do
                    run' mockDB (insert prsdQuote'') `shouldBe` Left (NonExistingDataError "Jomes")
            context "When input is ambiguous" $ do
                it "Throw an error when inserting a source with ambiguous authors" $ do
                    run' mockDB (insert prsTurn'') `shouldBe` Left (AmbiguousDataError (map value ambiguousHenry))
        describe "Searching" $ do
            context "When user input does not match to anything" $ do
                it "Return no value for a search by word" $ do
                    run' mockDB (searchWord noMatch) `shouldBe` (Right [])
                it "Return no value for search by tags" $ do
                    pending
            context "When user input do match something" $ do
                it "Return every value for a search by word" $ do
                    run' mockDB (searchWord oneMatch) `shouldBe` (Right (map value matchingChild))
                it "Return every value for a search by tag" $ do
                    pending
