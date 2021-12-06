module Action.WaitSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newCellMap, status),
                                       ActionStatus (Ok))
import           Action.Wait          (waitAction)
import           Control.Monad.Writer (writer)
import           Linear.V2            (V2 (V2))
import           SetUp                (initCellMap, initTileCollection)
import           Test.Hspec           (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "WaitAction" $
    it "returns a Ok result." $ result `shouldBe` expected
  where
    result = waitAction playerPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult {status = Ok, newCellMap = initCellMap, killed = []}
    expectedLog = []
    playerPosition = V2 0 0
