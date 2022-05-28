module Gimlight.Data.ListSpec
    ( spec
    ) where

import           Gimlight.Data.List (filterAll)
import           Test.Hspec         (Spec, describe, it, shouldBe)

spec :: Spec
spec = testFilterAll

testFilterAll :: Spec
testFilterAll =
    describe "filterAll" $
    it "filters with functions in a list" $
    filterAll [(> 3), (< 6)] [1 :: Int .. 10] `shouldBe` [4, 5]
