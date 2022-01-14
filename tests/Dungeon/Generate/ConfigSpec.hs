module Dungeon.Generate.ConfigSpec
    ( spec
    ) where

import           Control.Exception       (evaluate)
import           Dungeon.Generate.Config (config, mapHeightIsTooSmall,
                                          mapWidthIsTooSmall,
                                          roomMinIsLargerThanRoomMax)
import           Linear.V2               (V2 (V2))
import           Test.Hspec              (Spec, describe, errorCall, it,
                                          shouldThrow)

spec :: Spec
spec = do
    testPanicIfWidthIsTooSmall
    testPanicIfHeightIsTooSmall
    testPanicIfRoomMinSizeIsLargerThanRoomMaxSize

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

testPanicIfRoomMinSizeIsLargerThanRoomMaxSize :: Spec
testPanicIfRoomMinSizeIsLargerThanRoomMaxSize =
    describe "config" $
    it "panics if the given room minimum size is larger than the room maximum size" $
    evaluate (config 1 1 2 1 (V2 100 100)) `shouldThrow`
    errorCall (roomMinIsLargerThanRoomMax 2 1)
