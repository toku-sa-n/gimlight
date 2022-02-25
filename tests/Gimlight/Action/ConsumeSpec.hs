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
import           Gimlight.Actor                (Actor, equip, inventoryItems)
import qualified Gimlight.Actor                as A
import           Gimlight.ActorSpec            (addItems, removeItem)
import           Gimlight.Dungeon.Map.Cell     (CellMap, locateActorAt,
                                                removeActorAt)
import           Gimlight.Dungeon.Map.CellSpec (emptyCellMap, locateItemsActors,
                                                locateItemsActorsST)
import           Gimlight.IndexGenerator       (generator)
import           Gimlight.Inventory            (removeNthItem)
import           Gimlight.Item.Defined         (herb, sampleBook, sword,
                                                woodenArmor)
import           Gimlight.Item.Heal            (getHealAmount)
import           Gimlight.Item.SomeItem        (SomeItem)
import qualified Gimlight.Localization.Texts   as T
import           Gimlight.SetUp.CellMap        (mockTileCollection,
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
    result = consumeAction 0 playerPosition mockTileCollection cm
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            { status = ReadingStarted T.sampleBookContent
            , newCellMap = cm
            , killed = []
            }
    expectedLog = []
    cm = testMap $ liftUnion sampleBook

testConsumeHerb :: Spec
testConsumeHerb =
    it "returns a Ok result if an actor uses a herb" $
    result `shouldBe` expected
  where
    result = consumeAction 0 (V2 0 0) mockTileCollection cm
    expected = writer (expectedResult, expectedLog)
    expectedResult = okResult cellMapAfterConsuming
    expectedLog = [T.healed T.player $ getHealAmount herb]
    cellMapAfterConsuming =
        fromRight' $
        flip execStateT cm $ do
            a <- removeActorAt (V2 0 0)
            locateActorAt
                mockTileCollection
                (a & inventoryItems %~ (snd . removeNthItem 0))
                (V2 0 0)
    cm = testMap $ liftUnion herb

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
    cm = testMap $ liftUnion sword

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
    cm = testMap $ liftUnion woodenArmor

testMap :: SomeItem -> CellMap
testMap x =
    locateItemsActors [(V2 0 0, liftUnion $ player x)] $ emptyCellMap $ V2 1 1

player :: SomeItem -> Actor
player x = addItems [x] $ evalState A.player generator
