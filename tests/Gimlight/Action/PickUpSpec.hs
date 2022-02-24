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
                                              orcWithFullItemsPosition,
                                              orcWithoutItemsPosition)
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
    result = pickUpAction (V2 0 0) initTileCollection cm'
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Ok, newCellMap = cellMapAfterPickingUp, killed = []}
    cellMapAfterPickingUp =
        expectRight "Failed to pick up." $
        flip execStateT cm' $ do
            _ <- removeItemAt (V2 0 0)
            _ <- removeActorAt (V2 0 0)
            locateActorAt initTileCollection actorWithItem (V2 0 0)
    expectedLog = [T.youGotItem $ getName herb]
    actorWithItem =
        (\(x, _) -> x & inventoryItems %~ (fromJust . addItem (liftUnion herb)))
            (expectRight
                 "Failed to add an item."
                 (flip runStateT cm' $ removeActorAt (V2 0 0)))
    cm' =
        fromRight' $
        flip execStateT cm $ do
            locateActorAt
                initTileCollection
                (evalState player generator)
                (V2 0 0)
            locateItemAt initTileCollection (liftUnion herb) (V2 0 0)
    cm =
        cellMap $ array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer Nothing Nothing)]

testPickUpVoid :: Spec
testPickUpVoid =
    it "returns a Failed result if there is no item at the actor's foot." $
    result `shouldBe` expected
  where
    result = pickUpAction orcWithoutItemsPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult {status = Failed, newCellMap = initCellMap, killed = []}
    expectedLog = [T.youGotNothing]

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
