module Gimlight.Action.ConsumeSpec
    ( spec
    ) where

import           Control.Lens                  ((%~), (&))
import           Control.Monad.State           (evalState, execStateT)
import           Control.Monad.Writer          (writer)
import           Data.Either.Combinators       (fromRight')
import           Data.Maybe                    (fromJust)
import           Data.OpenUnion                (liftUnion)
import           Gimlight.Action               (ActionResultWithLog)
import           Gimlight.Action.Consume       (consumeAction)
import           Gimlight.ActionSpec           (okResult, readingResult)
import           Gimlight.Actor                (Actor, equip, inventoryItems)
import qualified Gimlight.Actor                as A
import           Gimlight.ActorSpec            (addItems, removeItem)
import           Gimlight.Coord                (Coord)
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
import           Gimlight.SetUp.CellMap        (mockTileCollection)
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
    result cm `shouldBe` expected
  where
    expected = writer (readingResult T.sampleBookContent cm, [])
    cm = testMap $ liftUnion sampleBook

testConsumeHerb :: Spec
testConsumeHerb =
    it "returns a Ok result if an actor uses a herb" $
    result cm `shouldBe` expected
  where
    expected =
        writer (okResult cmAfter, [T.healed T.player $ getHealAmount herb])
    cmAfter =
        fromRight' $
        flip execStateT cm $ do
            a <- removeActorAt playerPos
            locateItemsActorsST [(playerPos, liftUnion $ removeItem 0 a)]
    cm = testMap $ liftUnion herb

testEquipWeapon :: Spec
testEquipWeapon =
    it "returns a Ok result if an actor equips a weapon" $
    result cm `shouldBe` expected
  where
    expected =
        writer (okResult cellMapAfterEquipping, [T.equipped T.player T.sword])
    cellMapAfterEquipping =
        fromRight' $
        flip execStateT cm $ do
            a <- removeActorAt playerPos
            locateActorAt
                mockTileCollection
                (fromJust (equip (liftUnion sword) a) &
                 inventoryItems %~ (snd . removeNthItem 0))
                (V2 0 0)
    cm = testMap $ liftUnion sword

testEquipArmor :: Spec
testEquipArmor =
    it "returns a Ok result if an actor equips a weapon" $
    result cm `shouldBe` expected
  where
    expected = writer (okResult cmAfter, [T.equipped T.player T.woodenArmor])
    cmAfter =
        fromRight' $
        flip execStateT cm $ do
            a <- removeActorAt playerPos
            locateItemsActorsST
                [ ( playerPos
                  , liftUnion
                        (removeItem 0 $
                         fromJust $ equip (liftUnion woodenArmor) a))
                ]
    cm = testMap $ liftUnion woodenArmor

result :: CellMap -> ActionResultWithLog
result = consumeAction 0 playerPos mockTileCollection

testMap :: SomeItem -> CellMap
testMap x =
    locateItemsActors [(playerPos, liftUnion $ player x)] $
    emptyCellMap $ V2 1 1

player :: SomeItem -> Actor
player x = addItems [x] $ evalState A.player generator

playerPos :: Coord
playerPos = V2 0 0
