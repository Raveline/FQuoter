module FQuoter.Serialize.SerializeSpec(main, spec) where

import Test.Hspec
-- todo
main :: IO()
main = hspec spec

spec = do
        describe "nothing" $ do
            it "Does nothing right now." $ do
                1 `shouldBe` 1
