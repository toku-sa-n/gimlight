{-# LANGUAGE QuasiQuotes #-}

module Gimlight.Data.ListSpec
    ( spec
    ) where

import           Data.String.QQ     (s)
import           Gimlight.Data.List (intercalateIncludingHeadTail, makeTable)
import           Test.Hspec         (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    testIntercalateIncludingHeadTail
    testMakeTableWithFilledTable
    testMakeTableContainingEmptyList

testIntercalateIncludingHeadTail :: Spec
testIntercalateIncludingHeadTail =
    describe "intercalateIncludingHeadTail" $
    it "inserts a list between lists of the second argument, append it, prepend it, and concat them." $
    intercalateIncludingHeadTail "|" ["Ester", "Menyahnya", "Shinobu"] `shouldBe`
    "|Ester|Menyahnya|Shinobu|"

testMakeTableWithFilledTable :: Spec
testMakeTableWithFilledTable =
    describe "makeTable" $
    it "fills all cells if the all rows have the same length." $
    result `shouldBe` expected
  where
    result =
        makeTable
            [["Derich", "Rosemary", "Beroberos"], ["Hapico", "Mussle", "Fuku"]]
    expected =
        [s|
+---------+---------+---------+
|Derich   |Rosemary |Beroberos|
+---------+---------+---------+
|Hapico   |Mussle   |Fuku     |
+---------+---------+---------+|]

testMakeTableContainingEmptyList :: Spec
testMakeTableContainingEmptyList =
    describe "makeTable" $
    it "leaves cells as blank if a list is shorter than the longest one." $
    result `shouldBe` expected
  where
    result = makeTable [[], ["Foo", "Bar"], ["Baz"]]
    expected =
        [s|
+---+---+
|   |   |
+---+---+
|Foo|Bar|
+---+---+
|Baz|   |
+---+---+|]
