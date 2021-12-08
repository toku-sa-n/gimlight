{-# LANGUAGE OverloadedStrings #-}

module Action.MeleeSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newCellMap, status),
                                       ActionStatus (Ok))
import           Action.Melee         (meleeAction)
import           Actor                (Actor, attackFromTo, monster)
import           Actor.Identifier     (Identifier (Orc))
import qualified Actor.Status         as S
import           Actor.Status.Hp      (hp)
import           Control.Monad.Writer (runWriter, writer)
import           Data.Array           (array)
import           Data.Maybe           (fromJust)
import           Dungeon.Map.Cell     (CellMap, TileIdLayer (TileIdLayer),
                                       cellMap, locateActorAt, removeActorAt)
import           IndexGenerator       (IndexGenerator, generator)
import           Linear.V2            (V2 (V2))
import           SetUp                (initTileCollection)
import           Test.Hspec           (Spec, describe, it, shouldBe)

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
    result = meleeAction (V2 0 1) (V2 1 2) initTileCollection cm
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            { status = Ok
            , newCellMap = cellMapWithoutDefender
            , killed = [defender]
            }
    ((_, newDefender), expectedLog) = runWriter $ attackFromTo attacker defender
    (defender, cellMapWithoutDefender) =
        case removeActorAt (V2 1 3) cm of
            Just (a, ncm) -> (a, ncm)
            Nothing       -> error "unreachable."
    (attacker, _) = fromJust $ removeActorAt (V2 1 2) cm
    (cm, _) = initCellMap

testDamage :: Spec
testDamage =
    describe "Strongest orc" $
    it "attacks to the intermediate orc" $ result `shouldBe` expected
  where
    result = meleeAction (V2 (-1) 1) (V2 1 2) initTileCollection cm
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            { status = Ok
            , newCellMap =
                  fromJust $
                  locateActorAt
                      (fromJust newDefender)
                      (V2 0 3)
                      cellMapWithoutDefender
            , killed = []
            }
    ((_, newDefender), expectedLog) = runWriter $ attackFromTo attacker defender
    (defender, cellMapWithoutDefender) =
        case removeActorAt (V2 0 3) cm of
            Just (a, ncm) -> (a, ncm)
            Nothing       -> error "unreachable"
    attacker = fst $ fromJust $ removeActorAt (V2 1 2) cm
    (cm, _) = initCellMap

initCellMap :: (CellMap, IndexGenerator)
initCellMap = (fromJust afterLocating, g''')
  where
    afterLocating =
        locateActorAt w (V2 1 3) cm >>= locateActorAt i (V2 0 3) >>=
        locateActorAt s (V2 1 2)
    cm =
        cellMap $
        array (V2 0 0, V2 1 3) [(V2 x y, walkable) | x <- [0, 1], y <- [0 .. 3]]
    walkable = TileIdLayer (Just 0) (Just 0)
    (w, g') = weakest generator
    (i, g'') = intermediate g'
    (s, g''') = strongest g''

strongest :: IndexGenerator -> (Actor, IndexGenerator)
strongest g = monster g Orc (S.status (hp 100) 100 100) ""

intermediate :: IndexGenerator -> (Actor, IndexGenerator)
intermediate g = monster g Orc (S.status (hp 100) 50 50) ""

weakest :: IndexGenerator -> (Actor, IndexGenerator)
weakest g = monster g Orc (S.status (hp 1) 0 0) ""
