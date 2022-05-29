module Gimlight.Generator
    ( generateNonPositive
    , generatePositiveBigSmallNumbers
    ) where

import           Gimlight.Prelude
import           Test.QuickCheck  (Arbitrary (arbitrary), Gen, suchThat)

generateNonPositive :: Gen Int
generateNonPositive = negate . abs <$> arbitrary

generatePositiveBigSmallNumbers :: Gen (Int, Int)
generatePositiveBigSmallNumbers = arbitrary `suchThat` \(a, b) -> a > b && b > 0
