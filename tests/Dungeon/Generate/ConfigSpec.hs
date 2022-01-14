module Dungeon.Generate.ConfigSpec
    ( spec
    ) where

import           Control.Exception       (evaluate)
import           Dungeon.Generate.Config (config)
import           Linear.V2               (V2 (V2))
import           Test.Hspec              (Spec, describe, errorCall, it,
                                          shouldThrow)
import           UI.Draw.Config          (tileColumns, tileRows)

spec :: Spec
spec = do
    testPanicIfWidthIsTooSmall
    testPanicIfHeightIsTooSmall

testPanicIfWidthIsTooSmall :: Spec
testPanicIfWidthIsTooSmall =
    describe "config" $
    it "panics if the given map size is too small" $
    evaluate (config 1 1 1 1 (V2 width 1)) `shouldThrow`
    errorCall
        ("Map width is expected to be larger than or equal to " ++
         show tileColumns ++ " but the actual value is " ++ show width ++ ".")
  where
    width = 1

testPanicIfHeightIsTooSmall :: Spec
testPanicIfHeightIsTooSmall =
    describe "config" $
    it "panics if the given map size is too small" $
    evaluate (config 1 1 1 1 (V2 100 height)) `shouldThrow`
    errorCall
        ("Map height is expected to be larger than or equal to " ++
         show tileRows ++ " but the actual value is " ++ show height ++ ".")
  where
    height = 1
