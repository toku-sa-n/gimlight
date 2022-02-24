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
import           Gimlight.Coord              (Coord)
import           Gimlight.Dungeon.Map.Cell   (CellMap,
                                              TileIdLayer (TileIdLayer),
                                              cellMap, locateActorAt,
                                              locateItemAt, removeActorAt,
                                              removeItemAt)
import           Gimlight.IndexGenerator     (generator)
import           Gimlight.Inventory          (addItem, maxSlot)
import           Gimlight.Item               (getName)
import           Gimlight.Item.Defined       (herb)
import qualified Gimlight.Localization.Texts as T
import           Gimlight.SetUp.CellMap      (initTileCollection)
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
    result `shouldBe`
    expected
  where
    result = pickUpAction playerPos initTileCollection cm'
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Ok, newCellMap = cellMapAfterPickingUp, killed = []}
    cellMapAfterPickingUp =
        fromRight' $ flip execStateT cm' $ do
            _ <- removeItemAt playerPos
            _ <- removeActorAt playerPos
            locateActorAt initTileCollection actorWithItem playerPos
    expectedLog = [T.youGotItem $ getName herb]
    actorWithItem =
        (\(x, _) -> x & inventoryItems %~ (fromJust . addItem (liftUnion herb)))
            (fromRight' $ flip runStateT cm' $ removeActorAt playerPos)
    cm' =
        fromRight' $ flip execStateT cellMapWithPlayer $
        locateItemAt initTileCollection (liftUnion herb) playerPos

testPickUpVoid :: Spec
testPickUpVoid =
    it "returns a Failed result if there is no item at the actor's foot." $
    result `shouldBe`
    expected
  where
    result = pickUpAction playerPos initTileCollection cellMapWithPlayer
    expected = writer (expectedResult, expectedLog)
    expectedResult = failedResult cellMapWithPlayer
    expectedLog = [T.youGotNothing]

testPickUpWhenInventoryIsFull :: Spec
testPickUpWhenInventoryIsFull =
    it "returns a Failed result if the actor's inventory is full." $ result `shouldBe`
    expected
  where
    result = pickUpAction playerPos initTileCollection cm
    expected = writer (expectedResult, expectedLog)
    expectedResult = failedResult cm
    expectedLog = [T.bagIsFull]
    cm =
        fromRight' $ flip execStateT emptyCellMap $ do
            locateItemAt initTileCollection (liftUnion herb) playerPos
            locateActorAt
                initTileCollection
                (iterate
                     (inventoryItems %~ fromJust . addItem (liftUnion herb))
                     (evalState player generator) !!
                 maxSlot)
                playerPos

failedResult :: CellMap -> ActionResult
failedResult cm = ActionResult {status = Failed, newCellMap = cm, killed = []}

cellMapWithPlayer :: CellMap
cellMapWithPlayer =
    fromRight' $ flip execStateT emptyCellMap $
    locateActorAt initTileCollection (evalState player generator) playerPos

emptyCellMap :: CellMap
emptyCellMap =
    cellMap $ array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer Nothing Nothing)]

playerPos :: Coord
playerPos = V2 0 0
