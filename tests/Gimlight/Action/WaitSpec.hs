module Gimlight.Action.WaitSpec
    ( spec
    ) where

import           Control.Monad.Writer   (writer)
import           Gimlight.Action.Wait   (waitAction)
import           Gimlight.ActionSpec    (okResult)
import           Gimlight.SetUp.CellMap (initCellMap, mockTileCollection,
                                         playerPosition)
import           Test.Hspec             (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "WaitAction" $
    it "returns a Ok result." $ result `shouldBe` expected
  where
    result = waitAction playerPosition mockTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult = okResult initCellMap
    expectedLog = []
