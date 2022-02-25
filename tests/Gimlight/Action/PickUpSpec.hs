{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Gimlight.Action.PickUpSpec
    ( spec
    ) where

import           Control.Lens                  (over)
import           Control.Monad.State           (evalState, execStateT)
import           Control.Monad.Writer          (writer)
import           Data.Either.Combinators       (fromRight')
import           Data.Maybe                    (fromJust)
import           Data.OpenUnion                (Union, liftUnion,
                                                typesExhausted, (@>))
import           Gimlight.Action               (ActionResultWithLog)
import           Gimlight.Action.PickUp        (pickUpAction)
import           Gimlight.ActionSpec           (failedResult, okResult)
import           Gimlight.Actor                (Actor, inventoryItems)
import qualified Gimlight.Actor                as A
import           Gimlight.Coord                (Coord)
import           Gimlight.Dungeon.Map.Cell     (CellMap, locateActorAt,
                                                locateItemAt, removeActorAt,
                                                removeItemAt)
import           Gimlight.Dungeon.Map.CellSpec (emptyCellMap)
import           Gimlight.IndexGenerator       (generator)
import           Gimlight.Inventory            (addItem, maxSlot)
import           Gimlight.Item.Defined         (herb)
import           Gimlight.Item.SomeItem        (SomeItem)
import qualified Gimlight.Localization.Texts   as T
import           Gimlight.SetUp.CellMap        (initTileCollection)
import           Linear                        (V2 (V2))
import           Test.Hspec                    (Spec, it, shouldBe)

spec :: Spec
spec = do
    testPickUpSuccess
    testPickUpVoid
    testPickUpWhenInventoryIsFull

testPickUpSuccess :: Spec
testPickUpSuccess =
    it "returns a Ok result if there is an item at the actor's foot, and player's inventory is not full." $
    result cm `shouldBe` expected
  where
    expected = writer (okResult cellMapAfterPickingUp, [T.youGotItem T.herb])
    cellMapAfterPickingUp =
        fromRight' $
        flip execStateT cm $ do
            _ <- removeItemAt playerPos
            _ <- removeActorAt playerPos
            locateActorAt initTileCollection actorWithItem playerPos
    actorWithItem = addItems [liftUnion herb] player
    cm =
        cellMapWith
            [ (playerPos, liftUnion (liftUnion herb :: SomeItem))
            , (playerPos, liftUnion player)
            ]

testPickUpVoid :: Spec
testPickUpVoid =
    it "returns a Failed result if there is no item at the actor's foot." $
    result cm `shouldBe` expected
  where
    expected = writer (failedResult cm, [T.youGotNothing])
    cm = cellMapWith [(playerPos, liftUnion player)]

testPickUpWhenInventoryIsFull :: Spec
testPickUpWhenInventoryIsFull =
    it "returns a Failed result if the actor's inventory is full." $
    result cm `shouldBe` expected
  where
    expected = writer (failedResult cm, [T.bagIsFull])
    cm =
        cellMapWith
            [ (playerPos, liftUnion (liftUnion herb :: SomeItem))
            , (playerPos, liftUnion $ addItems items player)
            ]
    items = replicate maxSlot $ liftUnion herb

result :: CellMap -> ActionResultWithLog
result = pickUpAction playerPos initTileCollection

cellMapWith :: [(Coord, Union '[ Actor, SomeItem])] -> CellMap
cellMapWith xs = locateItemsActors xs testMap

locateItemsActors :: [(Coord, Union '[ Actor, SomeItem])] -> CellMap -> CellMap
locateItemsActors xs cm = foldl helper cm xs
  where
    helper ncm (pos, x) =
        fromRight' $
        flip execStateT ncm $
        (itemFunc pos @> actorFunc pos @> typesExhausted) x
    actorFunc = apply locateActorAt
    itemFunc = apply locateItemAt
    apply f pos x = f initTileCollection x pos

addItems :: [SomeItem] -> Actor -> Actor
addItems xs a = foldr (\x -> over inventoryItems (fromJust . addItem x)) a xs

testMap :: CellMap
testMap = emptyCellMap $ V2 1 1

player :: Actor
player = evalState A.player generator

playerPos :: Coord
playerPos = V2 0 0
