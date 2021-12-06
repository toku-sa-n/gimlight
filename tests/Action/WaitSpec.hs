module Action.WaitSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newCellMap, status),
                                       ActionStatus (Ok))
import           Action.Wait          (waitAction)
import           Actor                (player)
import           Control.Monad.Writer (writer)
import           Data.Maybe           (fromJust)
import           Dungeon.Map.Cell     (locateActorAt)
import           IndexGenerator       (generator)
import           Linear.V2            (V2 (V2))
import           SetUp                (initCellMap, initTileCollection)
import           Test.Hspec           (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "WaitAction" $
    it "returns a Ok result." $ result `shouldBe` expected
  where
    result = waitAction playerPosition initTileCollection cellMapWithPlayer
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult {status = Ok, newCellMap = cellMapWithPlayer, killed = []}
    expectedLog = []
    p = fst $ player generator
    cellMapWithPlayer = fromJust $ locateActorAt p playerPosition initCellMap
    playerPosition = V2 0 0
