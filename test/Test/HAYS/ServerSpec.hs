module Test.HAYS.ServerSpec
    ( spec
    ) where

import           Test.Hspec

spec :: Spec
spec = do
  describe "Server" $ do
    describe "Instances" $ do
      describe "Functor" $ do
        describe "fmap" $ do
          it "correctly modifies a value" $ do
            let a = (+ 1) <$> Just 0
            a `shouldBe` Just 1
