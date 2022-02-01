module Gimlight.Data.EitherSpec
    ( spec
    ) where

import           Control.Exception    (evaluate)
import           Gimlight.Data.Either (expectRight)
import           Test.Hspec           (Spec, describe, errorCall, it, shouldBe,
                                       shouldThrow)

spec :: Spec
spec = testExpectRight

testExpectRight :: Spec
testExpectRight =
    describe "expectRight" $ do
        it "returns the inner value if `Right` is passed." $
            expectRight msg (Right "Sushi" :: Either String String) `shouldBe`
            "Sushi"
        it "panics with the error message if `Left` is passed." $
            evaluate (expectRight msg (Left "Pizza")) `shouldThrow`
            errorCall "Why not sushi!: \"Pizza\""
  where
    msg = "Why not sushi!"
