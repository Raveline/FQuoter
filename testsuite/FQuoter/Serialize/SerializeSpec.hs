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

type MockDB = Map.Map String [DBValue SerializedType]

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
tprocess' (Free (Search DBAuthor (ByName s) n)) = do v <- ask
                                                     res <- return $ retrieve v s
                                                     tprocess (n res)
tprocess' (Free (LastInsert n)) = tprocess (n 0)

retrieve :: InfoValue -> String -> [DBValue SerializedType]
retrieve (ValueDictionary dic) s = Map.findWithDefault [] s dic
retrive _ _ = error "Does not have a mock db, cannot retrieve. Test badly designed."

execute :: InfoValue -> FalliableSerialization InfoValue Identity () -> ()
execute inf f = case execution of
                Right () -> ()
                Left err -> error (show err)
    where execution = runIdentity $ runExceptT $ runReaderT (tprocess f) inf

-- Test data 
hJames = Author (Just "Henry") (Just "James") Nothing
prsHjames = PAuthor $ hJames
prsTurn = PSource $ ParserSource "The turn of the screw" ["James"] Map.empty

mockDB = ValueDictionary $ Map.fromList [("James", [DBValue 1 (SAuthor hJames)])]

spec = do
        describe "Check insertion logic" $ do
            it "Make sure inserting an author will return nothing." $ do
                 execute NoValue (insert prsHjames)  `shouldBe` ()
            it "Make sure insertin a source when author does exist will work" $ do
                execute mockDB (insert prsTurn) `shouldBe` ()
