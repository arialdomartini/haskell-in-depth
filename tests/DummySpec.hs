module DummySpec(spec) where

import Test.Hspec

spec :: Spec
spec = do
  it "just passes" $ do
    "friends" `shouldBe` "friends"