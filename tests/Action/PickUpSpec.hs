module Action.PickUpSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newDungeon, status),
                                       ActionStatus (Ok))
import           Action.PickUp        (pickUpAction)
import           Actor                (inventoryItems, player)
import           Actor.Inventory      (addItem)
import           Control.Lens         ((%~), (&))
import           Control.Monad.Writer (writer)
import           Data.Array           (array)
import           Data.Maybe           (fromJust)
import           Dungeon              (Dungeon, dungeon, popItemAt, pushActor,
                                       pushItem)
import           Dungeon.Identifier   (Identifier (Beaeve))
import           Dungeon.Map.Cell     (TileIdLayer (TileIdLayer), cellMap)
import           Dungeon.Map.Tile     (TileCollection, tile)
import           IndexGenerator       (generator)
import           Item                 (getName, herb)
import           Linear.V2            (V2 (V2))
import qualified Localization.Texts   as T
import           Test.Hspec           (Spec, it, shouldBe)

spec :: Spec
spec = testPickUpSuccess

testPickUpSuccess :: Spec
testPickUpSuccess =
    it "returns a Ok result if there is an item at the player's foot, and player's inventory is not full." $
    result `shouldBe` expected
  where
    result =
        pickUpAction
            playerPosition
            actorWithoutItem
            initTileCollection
            initDungeon
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Ok, newDungeon = dungeonAfterPickingUp, killed = []}
    dungeonAfterPickingUp =
        pushActor playerPosition actorWithItem $
        snd $ popItemAt playerPosition initDungeon
    expectedLog = [T.youGotItem $ getName herb]
    actorWithItem =
        actorWithoutItem & inventoryItems %~ (fromJust . addItem herb)
    (actorWithoutItem, _) = player generator
    playerPosition = V2 0 0

initDungeon :: Dungeon
initDungeon = pushItem (V2 0 0) herb $ dungeon cm Beaeve
  where
    cm =
        cellMap $
        array (V2 0 0, V2 1 0) [(V2 0 0, emptyTile), (V2 1 0, emptyTile)]
    emptyTile = TileIdLayer Nothing Nothing

initTileCollection :: TileCollection
initTileCollection = array (0, 0) [(0, tile True True)]
