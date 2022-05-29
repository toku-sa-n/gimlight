module Gimlight.Data.EitherSpec
    ( spec
    ) where

import           Control.Exception    (evaluate)
import           Data.Text            (unpack)
import           Gimlight.Data.Either (expectRight)
import           Gimlight.Prelude
import           Test.Hspec           (Spec, describe, errorCall, it, shouldBe,
                                       shouldThrow)

spec :: Spec
spec = testExpectRight

testExpectRight :: Spec
testExpectRight =
    describe "expectRight" $ do
        it "returns the inner value if `Right` is passed." $
            expectRight msg (Right r :: Either Text Text) `shouldBe` r
        it "panics with the error message if `Left` is passed." $
            evaluate (expectRight msg (Left p)) `shouldThrow`
            errorCall (unpack $ msg <> ": " <> showt p)
  where
    r = "Sushi"
    p = "Pizza" :: Text
    msg = "Why not sushi!"
