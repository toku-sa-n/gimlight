{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Action.DropSpec
    ( spec
    ) where

import           Control.Lens                  ((%~), (&))
import           Control.Monad.State           (evalState, execStateT)
import           Control.Monad.Trans.Writer    (writer)
import           Data.Either.Combinators       (fromRight')
import           Data.OpenUnion                (liftUnion)
import           Gimlight.Action.Drop          (dropAction)
import           Gimlight.ActionSpec           (failedResult, okResult)
import           Gimlight.Actor                (inventoryItems, player)
import           Gimlight.ActorSpec            (addItems)
import           Gimlight.Dungeon.Map.Cell     (CellMap, locateActorAt,
                                                locateItemAt, removeActorAt)
import           Gimlight.Dungeon.Map.CellSpec (emptyCellMap, locateItemsActors)
import           Gimlight.IndexGenerator       (generator)
import           Gimlight.Inventory            (removeNthItem)
import           Gimlight.Item                 (getName)
import           Gimlight.Item.Defined         (herb)
import           Gimlight.Item.SomeItem        (SomeItem)
import qualified Gimlight.Localization.Texts   as T
import           Gimlight.SetUp.CellMap        (mockTileCollection)
import           Linear                        (V2 (V2))
import           Test.Hspec                    (Spec, it, shouldBe)

spec :: Spec
spec = do
    testDropItemSuccessfully
    testItemAlreadyExists

testDropItemSuccessfully :: Spec
testDropItemSuccessfully =
    it "returns a Ok result if there is no item at the player's foot." $
    result `shouldBe` expected
  where
    result = dropAction 0 (V2 0 0) mockTileCollection testMap
    expected = writer (expectedResult, expectedLog)
    expectedResult = okResult cellMapAfterDropping
    cellMapAfterDropping =
        fromRight' $
        flip execStateT testMap $ do
            a <- removeActorAt (V2 0 0)
            locateActorAt
                mockTileCollection
                (a & inventoryItems %~ (snd . removeNthItem 0))
                (V2 0 0)
            locateItemAt mockTileCollection (liftUnion herb) (V2 0 0)
    expectedLog = [T.youDropped $ getName herb]

testItemAlreadyExists :: Spec
testItemAlreadyExists =
    it "returns a Failed result if there is already an item at the player's foot." $
    result `shouldBe` expected
  where
    result = dropAction 0 (V2 0 0) mockTileCollection cm
    expected = writer (expectedResult, expectedLog)
    expectedResult = failedResult cm
    expectedLog = [T.itemExists]
    cm =
        locateItemsActors
            [(V2 0 0, liftUnion (liftUnion herb :: SomeItem))]
            testMap

testMap :: CellMap
testMap = locateItemsActors [(V2 0 0, liftUnion p)] $ emptyCellMap $ V2 1 1
  where
    p = addItems [liftUnion herb] $ evalState player generator
