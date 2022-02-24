{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Gimlight.Action.PickUpSpec
    ( spec
    ) where

import           Control.Lens                (over)
import           Control.Monad.State         (evalState, execStateT)
import           Control.Monad.Writer        (writer)
import           Data.Array                  (array)
import           Data.Either.Combinators     (fromRight')
import           Data.Maybe                  (fromJust)
import           Data.OpenUnion              (Union, liftUnion, typesExhausted,
                                              (@>))
import           Gimlight.Action             (ActionResult (ActionResult, killed, newCellMap, status),
                                              ActionStatus (Failed, Ok))
import           Gimlight.Action.PickUp      (pickUpAction)
import           Gimlight.Actor              (Actor, inventoryItems)
import qualified Gimlight.Actor              as A
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
import           Gimlight.Item.SomeItem      (SomeItem)
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
    result `shouldBe` expected
  where
    result = pickUpAction playerPos initTileCollection cm
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Ok, newCellMap = cellMapAfterPickingUp, killed = []}
    cellMapAfterPickingUp =
        fromRight' $
        flip execStateT cm $ do
            _ <- removeItemAt playerPos
            _ <- removeActorAt playerPos
            locateActorAt initTileCollection actorWithItem playerPos
    expectedLog = [T.youGotItem $ getName herb]
    actorWithItem = addItems [liftUnion herb] player
    cm =
        cellMapWith
            [ (playerPos, liftUnion (liftUnion herb :: SomeItem))
            , (playerPos, liftUnion player)
            ]

testPickUpVoid :: Spec
testPickUpVoid =
    it "returns a Failed result if there is no item at the actor's foot." $
    result `shouldBe` expected
  where
    result = pickUpAction playerPos initTileCollection cm
    expected = writer (failedResult cm, [T.youGotNothing])
    cm = cellMapWith [(playerPos, liftUnion player)]

testPickUpWhenInventoryIsFull :: Spec
testPickUpWhenInventoryIsFull =
    it "returns a Failed result if the actor's inventory is full." $
    result `shouldBe` expected
  where
    result = pickUpAction playerPos initTileCollection cm
    expected = writer (failedResult cm, [T.bagIsFull])
    cm =
        cellMapWith
            [ (playerPos, liftUnion (liftUnion herb :: SomeItem))
            , (playerPos, liftUnion $ addItems items player)
            ]
    items = replicate maxSlot $ liftUnion herb

cellMapWith :: [(Coord, Union '[ Actor, SomeItem])] -> CellMap
cellMapWith xs = locateItemsActors xs emptyCellMap

locateItemsActors :: [(Coord, Union '[ Actor, SomeItem])] -> CellMap -> CellMap
locateItemsActors xs cm = foldl helper cm xs
  where
    helper ncm (pos, x) =
        fromRight' $ (itemFunc ncm pos @> actorFunc ncm pos @> typesExhausted) x
    actorFunc ncm pos x =
        flip execStateT ncm $ locateActorAt initTileCollection x pos
    itemFunc ncm pos x =
        flip execStateT ncm $ locateItemAt initTileCollection x pos

addItems :: [SomeItem] -> Actor -> Actor
addItems xs a = foldr (\x -> over inventoryItems (fromJust . addItem x)) a xs

failedResult :: CellMap -> ActionResult
failedResult cm = ActionResult {status = Failed, newCellMap = cm, killed = []}

emptyCellMap :: CellMap
emptyCellMap =
    cellMap $ array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer Nothing Nothing)]

player :: Actor
player = evalState A.player generator

playerPos :: Coord
playerPos = V2 0 0
