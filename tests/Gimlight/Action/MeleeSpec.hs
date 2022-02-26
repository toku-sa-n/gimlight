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
import           Gimlight.Action.Melee         (meleeAction)
import           Gimlight.ActionSpec           (okResult, okWithKilled)
import           Gimlight.Actor                (Actor, attackFromTo, monster)
import           Gimlight.Actor.Identifier     (Identifier (Orc))
import           Gimlight.Actor.Status         (Status, status)
import           Gimlight.Actor.Status.Hp      (hp)
import           Gimlight.Dungeon.Map.Cell     (CellMap, removeActorAt)
import           Gimlight.Dungeon.Map.CellSpec (emptyCellMap, locateItemsActors,
                                                locateItemsActorsST)
import           Gimlight.IndexGenerator       (IndexGenerator, generator)
import           Gimlight.SetUp.CellMap        (initCellMap,
                                                intermediateOrcPosition,
                                                mockTileCollection,
                                                strongestOrcPosition)
import           Linear                        (V2 (V2))
import           Test.Hspec                    (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    testKill
    testDamage

testKill :: Spec
testKill =
    describe "Strongest orc" $ do
        it "kills the weakest orc" $ result `shouldBe` expected
        it "returns a Nothing defender" $ newDefender `shouldBe` Nothing
  where
    result = meleeAction (V2 1 0) (V2 0 0) mockTileCollection cm
    expected = writer (expectedResult, expectedLog)
    expectedResult = okWithKilled cellMapWithoutDefender [defender]
    ((_, newDefender), expectedLog) = runWriter $ attackFromTo attacker defender
    (defender, cellMapWithoutDefender) =
        fromRight' $ flip runStateT cm $ removeActorAt $ V2 1 0
    attacker = fromRight' $ flip evalStateT cm $ removeActorAt $ V2 0 0
    cm = testMap $ status (hp 1) 0 0

testDamage :: Spec
testDamage =
    describe "Strongest orc" $
    it "attacks to the intermediate orc" $ result `shouldBe` expected
  where
    result =
        meleeAction offset strongestOrcPosition mockTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        okResult $
        fromRight' $
        flip execStateT cellMapWithoutDefender $
        locateItemsActorsST
            [(intermediateOrcPosition, liftUnion $ fromJust newDefender)]
    ((_, newDefender), expectedLog) = runWriter $ attackFromTo attacker defender
    (defender, cellMapWithoutDefender) =
        fromRight' $
        flip runStateT initCellMap $ removeActorAt intermediateOrcPosition
    attacker =
        fromRight' $
        flip evalStateT initCellMap $ removeActorAt strongestOrcPosition
    offset = intermediateOrcPosition - strongestOrcPosition

testMap :: Status -> CellMap
testMap st =
    locateItemsActors (zip [V2 0 0, V2 1 0] $ map liftUnion [a1, a2]) $
    emptyCellMap $ V2 2 1
  where
    (a1, a2) =
        flip evalState generator $
        (,) <$> testMonster (status (hp 1) 2 0) <*> testMonster st

testMonster :: Status -> State IndexGenerator Actor
testMonster st = monster Orc st ""
