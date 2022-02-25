module Gimlight.Action.ConsumeSpec
    ( spec
    ) where

import           Control.Lens                  ((%~), (&))
import           Control.Monad.Morph           (generalize)
import           Control.Monad.State           (evalState, execStateT,
                                                mapStateT)
import           Control.Monad.Writer          (writer)
import           Data.Either.Combinators       (fromRight')
import           Data.Maybe                    (fromJust)
import           Data.OpenUnion                (liftUnion)
import           Gimlight.Action               (ActionResult (ActionResult, killed, newCellMap, status),
                                                ActionStatus (ReadingStarted))
import           Gimlight.Action.Consume       (consumeAction)
import           Gimlight.ActionSpec           (okResult)
import           Gimlight.Actor                (equip, inventoryItems, player)
import           Gimlight.ActorSpec            (addItems, removeItem)
import           Gimlight.Dungeon.Map.Cell     (locateActorAt, removeActorAt)
import           Gimlight.Dungeon.Map.CellSpec (emptyCellMap, locateItemsActors,
                                                locateItemsActorsST)
import           Gimlight.IndexGenerator       (generator)
import           Gimlight.Inventory            (removeNthItem)
import           Gimlight.Item                 (getEffect)
import           Gimlight.Item.Defined         (herb, sampleBook, sword,
                                                woodenArmor)
import           Gimlight.Item.Heal            (getHealAmount)
import qualified Gimlight.Localization.Texts   as T
import           Gimlight.SetUp.CellMap        (initCellMap, mockTileCollection,
                                                orcWithHerbPosition,
                                                playerPosition)
import           Linear                        (V2 (V2))
import           Test.Hspec                    (Spec, it, shouldBe)

spec :: Spec
spec = do
    testStartReadingBook
    testConsumeHerb
    testEquipWeapon
    testEquipArmor

testStartReadingBook :: Spec
testStartReadingBook =
    it "returns a ReadingStarted result if an actor uses a book" $
    result `shouldBe` expected
  where
    result = consumeAction 0 playerPosition mockTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            { status = ReadingStarted $ getEffect sampleBook
            , newCellMap = initCellMap
            , killed = []
            }
    expectedLog = []

testConsumeHerb :: Spec
testConsumeHerb =
    it "returns a Ok result if an actor uses a herb" $
    result `shouldBe` expected
  where
    result = consumeAction 0 orcWithHerbPosition mockTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult = okResult cellMapAfterConsuming
    expectedLog = [T.healed T.orc $ getHealAmount herb]
    cellMapAfterConsuming =
        fromRight' $
        flip execStateT initCellMap $ do
            a <- removeActorAt orcWithHerbPosition
            locateActorAt
                mockTileCollection
                (a & inventoryItems %~ (snd . removeNthItem 0))
                orcWithHerbPosition

testEquipWeapon :: Spec
testEquipWeapon =
    it "returns a Ok result if an actor equips a weapon" $
    result `shouldBe` expected
  where
    result = consumeAction 0 (V2 0 0) mockTileCollection cm
    expected =
        writer (okResult cellMapAfterEquipping, [T.equipped T.player T.sword])
    cellMapAfterEquipping =
        fromRight' $
        flip execStateT cm $ do
            a <- removeActorAt (V2 0 0)
            locateActorAt
                mockTileCollection
                (fromJust (equip (liftUnion sword) a) &
                 inventoryItems %~ (snd . removeNthItem 0))
                (V2 0 0)
    cm = locateItemsActors [(V2 0 0, liftUnion p)] $ emptyCellMap $ V2 1 1
    p = addItems [liftUnion sword] $ evalState player generator

testEquipArmor :: Spec
testEquipArmor =
    it "returns a Ok result if an actor equips a weapon" $
    result `shouldBe` expected
  where
    result = consumeAction 0 (V2 0 0) mockTileCollection cm
    expected = writer (expectedResult, expectedLog)
    expectedResult = okResult cellMapAfterEquipping
    expectedLog = [T.equipped T.player T.woodenArmor]
    cellMapAfterEquipping =
        fromRight' $
        flip execStateT cm $ do
            a <- removeActorAt (V2 0 0)
            mapStateT generalize $
                locateItemsActorsST
                    [ ( V2 0 0
                      , liftUnion
                            (removeItem 0 $
                             fromJust $ equip (liftUnion woodenArmor) a))
                    ]
    cm = locateItemsActors [(V2 0 0, liftUnion p)] $ emptyCellMap $ V2 1 1
    p = addItems [liftUnion woodenArmor] $ evalState player generator
