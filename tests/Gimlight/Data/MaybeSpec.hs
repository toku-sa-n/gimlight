module Gimlight.Data.MaybeSpec
    ( spec
    ) where

import           Control.Exception   (evaluate)
import           Data.Text           (unpack)
import           Gimlight.Data.Maybe (expectJust)
import           Gimlight.Prelude
import           Test.Hspec          (Spec, describe, errorCall, it, shouldBe,
                                      shouldThrow)

spec :: Spec
spec = testExpectJust

testExpectJust :: Spec
testExpectJust =
    describe "expectJust" $ do
        it "returns the inner value if it receives a `Just` value." $
            expectJust msg (Just v) `shouldBe` v
        it "panics if it receives a `Nothing` value." $
            evaluate (unpack $ expectJust msg Nothing) `shouldThrow`
            errorCall (unpack msg)
  where
    v = "Marion" :: Text
    msg = "We need a Marion."
