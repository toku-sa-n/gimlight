module Gimlight.System.PathSpec
    ( spec
    ) where

import           Gimlight.System.Path (canonicalizeToUnixStyleRelativePath)
import           Test.Hspec           (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = testCanonicalizeToUnixStyleRelativePath

testCanonicalizeToUnixStyleRelativePath :: Spec
testCanonicalizeToUnixStyleRelativePath = do
    result <- runIO $ canonicalizeToUnixStyleRelativePath "foo/bar"
    describe "canonicalizeToUnixStyleRelativePath" $
        it "converts a path to Unix-style relative path." $
        result `shouldBe` expected
  where
    expected = "foo/bar"
