{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Action.MeleeSpec
    ( spec
    ) where

import           Control.Lens                  (set)
import           Control.Monad.State           (State, StateT (runStateT),
                                                evalState, evalStateT,
                                                execStateT)
import           Control.Monad.Writer          (runWriter, writer)
import           Data.Either.Combinators       (fromRight')
import           Data.Maybe                    (fromJust)
import           Data.OpenUnion                (liftUnion)
import           Gimlight.Action               (ActionResultWithLog)
import           Gimlight.Action.Melee         (meleeAction)
import           Gimlight.ActionSpec           (okResult, okWithKilled)
import           Gimlight.Actor                (Actor, attackFromTo, facing,
                                                monster)
import           Gimlight.Actor.Identifier     (Identifier (Orc))
import           Gimlight.Actor.Status         (Status, status)
import           Gimlight.Actor.Status.Hp      (hp)
import           Gimlight.Coord                (Coord)
import           Gimlight.Direction            (Direction (East))
import           Gimlight.Dungeon.Map.Cell     (CellMap, removeActorAt)
import           Gimlight.Dungeon.Map.CellSpec (emptyCellMap, locateItemsActors,
                                                locateItemsActorsST, mapActorAt)
import           Gimlight.Dungeon.Map.TileSpec (mockTileCollection)
import           Gimlight.IndexGenerator       (IndexGenerator, generator)
import           Gimlight.Log                  (MessageLog)
import           Gimlight.Prelude
import           Linear                        (V2 (V2))
import           Test.Hspec                    (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "meleeAction" $ do
        testKill
        testDamage

testKill :: Spec
testKill = it "kills the weakest orc" $ result cm `shouldBe` expected
  where
    expected = writer (expectedResult, expectedLog)
    expectedResult = okWithKilled mapAfterAttack [defender]
    (_, expectedLog) = defenderAfterAttackAndLog cm
    mapAfterAttack =
        fromRight' $
        flip execStateT cellMapWithoutDefender $
        mapActorAt mockTileCollection atkPos (set facing East)
    (defender, cellMapWithoutDefender) = defenderAndMap cm
    cm = testMap $ status (hp 1) 0 0

testDamage :: Spec
testDamage =
    it "attacks to the intermediate orc" $ result cm `shouldBe` expected
  where
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        okResult $
        fromRight' $
        flip execStateT cellMapWithoutDefender $ do
            locateItemsActorsST [(defPos, liftUnion $ fromJust newDefender)]
            mapActorAt mockTileCollection atkPos (set facing East)
    (newDefender, expectedLog) = defenderAfterAttackAndLog cm
    (_, cellMapWithoutDefender) = defenderAndMap cm
    cm = testMap $ status (hp 2) 0 1

testMap :: Status -> CellMap
testMap st =
    locateItemsActors (zip [atkPos, defPos] $ map liftUnion [a1, a2]) $
    emptyCellMap $ V2 2 1
  where
    (a1, a2) =
        flip evalState generator $
        (,) <$> testMonster (status (hp 1) 2 0) <*> testMonster st

result :: CellMap -> ActionResultWithLog
result = meleeAction East atkPos mockTileCollection

defenderAfterAttackAndLog :: CellMap -> (Maybe Actor, MessageLog)
defenderAfterAttackAndLog cm = (d, l)
  where
    ((_, d), l) = runWriter $ attackFromTo (attacker cm) defender
    defender = fst $ defenderAndMap cm

attacker :: CellMap -> Actor
attacker cm = fromRight' $ flip evalStateT cm $ removeActorAt atkPos

defenderAndMap :: CellMap -> (Actor, CellMap)
defenderAndMap cm = fromRight' $ flip runStateT cm $ removeActorAt defPos

testMonster :: Status -> State IndexGenerator Actor
testMonster st = monster Orc st ""

atkPos :: Coord
atkPos = V2 0 0

defPos :: Coord
defPos = V2 1 0
