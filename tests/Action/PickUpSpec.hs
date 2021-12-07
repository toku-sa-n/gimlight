module Action.PickUpSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newCellMap, status),
                                       ActionStatus (Failed, Ok))
import           Action.PickUp        (pickUpAction)
import           Actor                (inventoryItems, player)
import           Actor.Inventory      (addItem)
import           Control.Lens         ((%~), (&))
import           Control.Monad.Writer (writer)
import           Data.Maybe           (fromJust)
import           Dungeon.Map.Cell     (locateActorAt, removeActorAt,
                                       removeItemAt)
import           IndexGenerator       (generator)
import           Item                 (getName, herb)
import qualified Localization.Texts   as T
import           SetUp                (initCellMap, initTileCollection,
                                       orcWithFullItemsPosition,
                                       orcWithoutItemsPosition, playerPosition)
import           Test.Hspec           (Spec, it, shouldBe)

spec :: Spec
spec = do
    testPickUpSuccess
    testPickUpVoid
    testPickUpWhenInventoryIsFull

testPickUpSuccess :: Spec
testPickUpSuccess =
    it "returns a Ok result if there is an item at the player's foot, and player's inventory is not full." $
    result `shouldBe` expected
  where
    result = pickUpAction playerPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Ok, newCellMap = cellMapAfterPickingUp, killed = []}
    cellMapAfterPickingUp =
        fromJust $
        removeItemAt playerPosition initCellMap >>=
        removeActorAt playerPosition . snd >>=
        locateActorAt actorWithItem playerPosition . snd
    expectedLog = [T.youGotItem $ getName herb]
    actorWithItem =
        actorWithoutItem & inventoryItems %~ (fromJust . addItem herb)
    (actorWithoutItem, _) = player generator

testPickUpVoid :: Spec
testPickUpVoid =
    it "returns a Failed result if there is no item at the player's foot." $
    result `shouldBe` expected
  where
    result = pickUpAction orcWithoutItemsPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult {status = Failed, newCellMap = initCellMap, killed = []}
    expectedLog = [T.youGotNothing]

testPickUpWhenInventoryIsFull :: Spec
testPickUpWhenInventoryIsFull =
    it "returns a Failed result if the player's inventory is full." $
    result `shouldBe` expected
  where
    result =
        pickUpAction orcWithFullItemsPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult {status = Failed, newCellMap = initCellMap, killed = []}
    expectedLog = [T.bagIsFull]
