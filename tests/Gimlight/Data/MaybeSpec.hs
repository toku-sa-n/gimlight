module Gimlight.Data.MaybeSpec
    ( spec
    ) where

import           Control.Exception   (evaluate)
import           Gimlight.Data.Maybe (expectJust)
import           Test.Hspec          (Spec, describe, errorCall, it, shouldBe,
                                      shouldThrow)

spec :: Spec
spec = testExpectJust

testExpectJust :: Spec
testExpectJust =
    describe "expectJust" $ do
        it "returns the inner value if it receives a `Just` value." $
            expectJust "We need a Marion." (Just "Marion") `shouldBe` "Marion"
        it "panics if it receives a `Nothing` value." $
            evaluate (expectJust "We need a Marion." Nothing) `shouldThrow`
            errorCall "We need a Marion."
