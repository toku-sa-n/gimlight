module Action.DropSpec
    ( spec
    ) where

import           Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = it "is the same value" $ (3 :: Integer) `shouldBe` (3 :: Integer)
