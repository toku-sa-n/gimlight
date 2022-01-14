module Dungeon.Generate.ConfigSpec
    ( spec
    ) where

import           Control.Exception       (evaluate)
import           Dungeon.Generate.Config (config, mapHeightIsTooSmall,
                                          mapWidthIsTooSmall)
import           Linear.V2               (V2 (V2))
import           Test.Hspec              (Spec, describe, errorCall, it,
                                          shouldThrow)

spec :: Spec
spec = do
    testPanicIfWidthIsTooSmall
    testPanicIfHeightIsTooSmall

testPanicIfWidthIsTooSmall :: Spec
testPanicIfWidthIsTooSmall =
    describe "config" $
    it "panics if the given map width is too small" $
    evaluate (config 1 1 1 1 (V2 width 1)) `shouldThrow`
    errorCall (mapWidthIsTooSmall width)
  where
    width = 1

testPanicIfHeightIsTooSmall :: Spec
testPanicIfHeightIsTooSmall =
    describe "config" $
    it "panics if the given map height is too small" $
    evaluate (config 1 1 1 1 (V2 100 height)) `shouldThrow`
    errorCall (mapHeightIsTooSmall height)
  where
    height = 1
