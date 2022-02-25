{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Action.MeleeSpec
    ( spec
    ) where

import           Control.Monad.State       (StateT (runStateT), evalStateT,
                                            execStateT)
import           Control.Monad.Writer      (runWriter, writer)
import           Data.Either.Combinators   (fromRight')
import           Data.Maybe                (fromJust)
import           Gimlight.Action           (ActionResult (ActionResult, killed, newCellMap, status),
                                            ActionStatus (Ok))
import           Gimlight.Action.Melee     (meleeAction)
import           Gimlight.ActionSpec       (okWithKilled)
import           Gimlight.Actor            (attackFromTo)
import           Gimlight.Dungeon.Map.Cell (locateActorAt, removeActorAt)
import           Gimlight.SetUp.CellMap    (initCellMap,
                                            intermediateOrcPosition,
                                            mockTileCollection,
                                            strongestOrcPosition,
                                            weakestOrcPosition)
import           Test.Hspec                (Spec, describe, it, shouldBe)

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
    result =
        meleeAction offset strongestOrcPosition mockTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult = okWithKilled cellMapWithoutDefender [defender]
    ((_, newDefender), expectedLog) = runWriter $ attackFromTo attacker defender
    (defender, cellMapWithoutDefender) =
        fromRight' $
        flip runStateT initCellMap $ removeActorAt weakestOrcPosition
    attacker =
        fromRight' $
        flip evalStateT initCellMap $ removeActorAt strongestOrcPosition
    offset = weakestOrcPosition - strongestOrcPosition

testDamage :: Spec
testDamage =
    describe "Strongest orc" $
    it "attacks to the intermediate orc" $ result `shouldBe` expected
  where
    result =
        meleeAction offset strongestOrcPosition mockTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            { status = Ok
            , newCellMap =
                  fromRight' $
                  flip execStateT cellMapWithoutDefender $
                  locateActorAt
                      mockTileCollection
                      (fromJust newDefender)
                      intermediateOrcPosition
            , killed = []
            }
    ((_, newDefender), expectedLog) = runWriter $ attackFromTo attacker defender
    (defender, cellMapWithoutDefender) =
        fromRight' $
        flip runStateT initCellMap $ removeActorAt intermediateOrcPosition
    attacker =
        fromRight' $
        flip evalStateT initCellMap $ removeActorAt strongestOrcPosition
    offset = intermediateOrcPosition - strongestOrcPosition
