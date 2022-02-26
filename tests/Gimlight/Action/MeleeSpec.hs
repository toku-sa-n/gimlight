{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Action.MeleeSpec
    ( spec
    ) where

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
import           Gimlight.Actor                (Actor, attackFromTo, monster)
import           Gimlight.Actor.Identifier     (Identifier (Orc))
import           Gimlight.Actor.Status         (Status, status)
import           Gimlight.Actor.Status.Hp      (hp)
import           Gimlight.Coord                (Coord)
import           Gimlight.Dungeon.Map.Cell     (CellMap, removeActorAt)
import           Gimlight.Dungeon.Map.CellSpec (emptyCellMap, locateItemsActors,
                                                locateItemsActorsST)
import           Gimlight.IndexGenerator       (IndexGenerator, generator)
import           Gimlight.SetUp.CellMap        (mockTileCollection)
import           Linear                        (V2 (V2))
import           Test.Hspec                    (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    testKill
    testDamage

testKill :: Spec
testKill =
    describe "Strongest orc" $ do
        it "kills the weakest orc" $ result cm `shouldBe` expected
        it "returns a Nothing defender" $ newDefender `shouldBe` Nothing
  where
    expected = writer (expectedResult, expectedLog)
    expectedResult = okWithKilled cellMapWithoutDefender [defender]
    ((_, newDefender), expectedLog) = runWriter $ attackFromTo attacker defender
    (defender, cellMapWithoutDefender) =
        fromRight' $ flip runStateT cm $ removeActorAt defPos
    attacker = fromRight' $ flip evalStateT cm $ removeActorAt atkPos
    cm = testMap $ status (hp 1) 0 0

testDamage :: Spec
testDamage =
    describe "Strongest orc" $
    it "attacks to the intermediate orc" $ result cm `shouldBe` expected
  where
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        okResult $
        fromRight' $
        flip execStateT cellMapWithoutDefender $
        locateItemsActorsST [(defPos, liftUnion $ fromJust newDefender)]
    ((_, newDefender), expectedLog) = runWriter $ attackFromTo attacker defender
    (defender, cellMapWithoutDefender) =
        fromRight' $ flip runStateT cm $ removeActorAt defPos
    attacker = fromRight' $ flip evalStateT cm $ removeActorAt atkPos
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
result = meleeAction offset atkPos mockTileCollection

testMonster :: Status -> State IndexGenerator Actor
testMonster st = monster Orc st ""

atkPos :: Coord
atkPos = V2 0 0

defPos :: Coord
defPos = V2 1 0

offset :: V2 Int
offset = defPos - atkPos
