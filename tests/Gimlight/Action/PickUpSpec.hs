module Gimlight.Action.PickUpSpec
    ( spec
    ) where

import           Control.Lens                ((%~), (&))
import           Control.Monad.State         (StateT (runStateT), evalState,
                                              execStateT)
import           Control.Monad.Writer        (writer)
import           Data.Array                  (array)
import           Data.Either.Combinators     (fromRight')
import           Data.Maybe                  (fromJust)
import           Data.OpenUnion              (liftUnion)
import           Gimlight.Action             (ActionResult (ActionResult, killed, newCellMap, status),
                                              ActionStatus (Failed, Ok))
import           Gimlight.Action.PickUp      (pickUpAction)
import           Gimlight.Actor              (inventoryItems, player)
import           Gimlight.Data.Either        (expectRight)
import           Gimlight.Dungeon.Map.Cell   (TileIdLayer (TileIdLayer),
                                              cellMap, locateActorAt,
                                              locateItemAt, removeActorAt,
                                              removeItemAt)
import           Gimlight.IndexGenerator     (generator)
import           Gimlight.Inventory          (addItem)
import           Gimlight.Item               (getName)
import           Gimlight.Item.Defined       (herb)
import qualified Gimlight.Localization.Texts as T
import           Gimlight.SetUp.CellMap      (initCellMap, initTileCollection,
                                              orcWithFullItemsPosition)
import           Linear                      (V2 (V2))
import           Test.Hspec                  (Spec, it, shouldBe)

spec :: Spec
spec = do
    testPickUpSuccess
    testPickUpVoid
    testPickUpWhenInventoryIsFull

testPickUpSuccess :: Spec
testPickUpSuccess =
    it "returns a Ok result if there is an item at the actor's foot, and player's inventory is not full." $
    result `shouldBe` expected
  where
    result = pickUpAction playerPos initTileCollection cm'
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Ok, newCellMap = cellMapAfterPickingUp, killed = []}
    cellMapAfterPickingUp =
        expectRight "Failed to pick up." $
        flip execStateT cm' $ do
            _ <- removeItemAt playerPos
            _ <- removeActorAt playerPos
            locateActorAt initTileCollection actorWithItem playerPos
    expectedLog = [T.youGotItem $ getName herb]
    actorWithItem =
        (\(x, _) -> x & inventoryItems %~ (fromJust . addItem (liftUnion herb)))
            (expectRight
                 "Failed to add an item."
                 (flip runStateT cm' $ removeActorAt playerPos))
    cm' =
        fromRight' $
        flip execStateT cm $ do
            locateActorAt
                initTileCollection
                (evalState player generator)
                playerPos
            locateItemAt initTileCollection (liftUnion herb) playerPos
    cm =
        cellMap $ array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer Nothing Nothing)]
    playerPos = V2 0 0

testPickUpVoid :: Spec
testPickUpVoid =
    it "returns a Failed result if there is no item at the actor's foot." $
    result `shouldBe` expected
  where
    result = pickUpAction playerPos initTileCollection cm'
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult {status = Failed, newCellMap = cm', killed = []}
    expectedLog = [T.youGotNothing]
    cm' =
        fromRight' $
        flip execStateT cm $
        locateActorAt initTileCollection (evalState player generator) playerPos
    cm =
        cellMap $ array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer Nothing Nothing)]
    playerPos = V2 0 0

testPickUpWhenInventoryIsFull :: Spec
testPickUpWhenInventoryIsFull =
    it "returns a Failed result if the actor's inventory is full." $
    result `shouldBe` expected
  where
    result =
        pickUpAction orcWithFullItemsPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult {status = Failed, newCellMap = initCellMap, killed = []}
    expectedLog = [T.bagIsFull]
