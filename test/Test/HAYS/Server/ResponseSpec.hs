module Test.HAYS.Server.ResponseSpec
    ( spec
    ) where

import           Test.Hspec

spec :: Spec
spec = do
  describe "Response" $ do
    describe "Instances" $ do
      describe "Functor" $ do
        describe "fmap" $ do
          it "correctly modifies a value" $ do
            let a = (+ 1) <$> Just 0
            a `shouldBe` Just 1
