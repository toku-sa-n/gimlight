module Gimlight.Data.EitherSpec
    ( spec
    ) where

import           Control.Exception    (evaluate)
import           Data.Text            (pack, unpack)
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
            expectRight msg (Right r :: Either String String) `shouldBe` r
        it "panics with the error message if `Left` is passed." $
            evaluate (expectRight msg (Left p)) `shouldThrow`
            errorCall (unpack $ msg <> ": " <> pack (show p))
  where
    r = "Sushi"
    p = "Pizza" :: Text
    msg = "Why not sushi!"
