module Dungeon.Generate.ConfigSpec
    ( spec
    ) where

import           Control.Exception       (evaluate)
import           Dungeon.Generate.Config (Config, config, maxRoomMustBePositive,
                                          numOfFloorsMustBePositive,
                                          roomMinIsLargerThanRoomMax,
                                          roomMinSizeIsLargerThanRoomHeight,
                                          roomMinSizeIsLargerThanRoomWidth,
                                          roomMinSizeMustBePositive)
import           Generator               (generateNonPositive,
                                          generatePositiveBigSmallNumbers)
import           Linear.V2               (V2 (V2))
import           Test.Hspec              (Spec, describe, errorCall, it,
                                          shouldThrow)
import           Test.QuickCheck         (forAll)

spec :: Spec
spec =
    describe "config" $ do
        testPanicIfNumOfFloorsIsNotPositive
        testPanicIfMaxRoomsIsNotPositive
        testPanicIfRoomMinSizeIsNotPositive
        testPanicIfRoomMinSizeIsLargerThanRoomMaxSize
        testPanicIfRoomMinSizeIsLargerThanMapWidth
        testPanicIfRoomMinSizeIsLargerThanMapHeight

testPanicIfNumOfFloorsIsNotPositive :: Spec
testPanicIfNumOfFloorsIsNotPositive =
    it "panics if the given number of floors is not positive." $
    forAll generateNonPositive $ \n ->
        evaluate (config n 1 1 1 (V2 100 100)) `shouldThrow`
        errorCall numOfFloorsMustBePositive

testPanicIfMaxRoomsIsNotPositive :: Spec
testPanicIfMaxRoomsIsNotPositive =
    it "panics if the given number of maximum rooms is not positive." $
    forAll generateNonPositive $ \n ->
        evaluate (config 1 n 1 1 (V2 100 100)) `shouldThrow`
        errorCall maxRoomMustBePositive

testPanicIfRoomMinSizeIsNotPositive :: Spec
testPanicIfRoomMinSizeIsNotPositive =
    it "panics if the given number of minimum room size is not positive." $
    forAll generateNonPositive $ \n ->
        evaluate (config 1 1 n 1 (V2 100 100)) `shouldThrow`
        errorCall roomMinSizeMustBePositive

testPanicIfRoomMinSizeIsLargerThanRoomMaxSize :: Spec
testPanicIfRoomMinSizeIsLargerThanRoomMaxSize =
    it "panics if the given room minimum size is larger than the room maximum size" $
    forAll generatePositiveBigSmallNumbers $ \(rmin, rmax) ->
        evaluate (config 1 1 rmin rmax (V2 100 100)) `shouldThrow`
        errorCall (roomMinIsLargerThanRoomMax rmin rmax)

testPanicIfRoomMinSizeIsLargerThanMapWidth :: Spec
testPanicIfRoomMinSizeIsLargerThanMapWidth =
    it "panics if the given room minimum size is larger than the room width" $
    evaluate (configFromMapSize $ V2 4 6) `shouldThrow`
    errorCall (roomMinSizeIsLargerThanRoomWidth 5 4)

testPanicIfRoomMinSizeIsLargerThanMapHeight :: Spec
testPanicIfRoomMinSizeIsLargerThanMapHeight =
    it "panics if the given room minimum size is larger than the room height" $
    evaluate (configFromMapSize $ V2 6 4) `shouldThrow`
    errorCall (roomMinSizeIsLargerThanRoomHeight 5 4)

configFromMapSize :: V2 Int -> Config
configFromMapSize = config 1 1 5 5
