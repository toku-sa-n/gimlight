module Gimlight.Data.StringSpec
    ( spec
    ) where

import           Gimlight.Data.String (adjustLength)
import           Test.Hspec           (Spec, describe, it, shouldBe)

spec :: Spec
spec = testAdjustLength

testAdjustLength :: Spec
testAdjustLength =
    describe "adjustLength" $
    it "appends spaces so that the length is adjusted to the given number." $
    adjustLength 10 "Marion" `shouldBe` "Marion    "
