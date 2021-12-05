module Action.ConsumeSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newDungeon, status),
                                       ActionStatus (ReadingStarted))
import           Action.Consume       (consumeAction)
import           Actor                (inventoryItems, player)
import           Actor.Inventory      (addItem)
import           Control.Lens         ((%~), (&))
import           Control.Monad.Writer (writer)
import           Data.Array           (array)
import           Data.Maybe           (fromJust)
import           Dungeon              (dungeon, pushActor)
import           Dungeon.Identifier   (Identifier (Beaeve))
import           Dungeon.Map.Cell     (TileIdLayer (TileIdLayer), cellMap)
import           Dungeon.Map.Tile     (tile)
import           IndexGenerator       (generator)
import           Item                 (Effect (Book), getEffect, sampleBook)
import           Linear.V2            (V2 (V2))
import           Test.Hspec           (Spec, it, shouldBe)

spec :: Spec
spec = testStartReadingBook

testStartReadingBook :: Spec
testStartReadingBook =
    it "returns a ReadingStarted result if an actor uses a book" $
    result `shouldBe` expected
  where
    result = consumeAction 0 playerPosition p tc dungeonWithoutPlayer
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            { status = ReadingStarted $ bookContent $ getEffect sampleBook
            , newDungeon = dungeonWithPlayer
            , killed = []
            }
    expectedLog = []
    dungeonWithPlayer = pushActor playerPosition p dungeonWithoutPlayer
    dungeonWithoutPlayer = dungeon cm Beaeve
    cm =
        cellMap $ array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer Nothing Nothing)]
    bookContent (Book c) = c
    bookContent _        = error "Not a book."
    tc = array (0, 0) [(0, tile True True)]
    p =
        fst (player generator) &
        inventoryItems %~ (fromJust . addItem sampleBook)
    playerPosition = V2 0 0
