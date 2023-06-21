module Test.HAYS.Server.RouterSpec
    ( spec
    ) where

import           Test.Hspec

spec :: Spec
spec = do
  describe "Router" $ do
    describe "Instances" $ do
      describe "Functor" $ do
        describe "fmap" $ do
          it "correctly modifies a value" $ do
            let a = (+ 1) <$> Just 0
            a `shouldBe` Just 1
