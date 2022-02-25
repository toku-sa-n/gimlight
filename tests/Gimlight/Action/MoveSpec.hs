module Gimlight.Action.MoveSpec
    ( spec
    ) where

import           Control.Monad.State         (evalState, execStateT)
import           Control.Monad.Writer        (writer)
import           Data.Array                  (array)
import           Data.Either.Combinators     (fromRight')
import           Gimlight.Action             (ActionResult (ActionResult, killed, newCellMap, status),
                                              ActionResultWithLog,
                                              ActionStatus (Ok))
import           Gimlight.Action.Move        (moveAction)
import           Gimlight.ActionSpec         (failedResult)
import           Gimlight.Actor.Monsters     (orc)
import           Gimlight.Dungeon.Map.Cell   (CellMap,
                                              TileIdLayer (TileIdLayer),
                                              cellMap, locateActorAt,
                                              removeActorAt)
import           Gimlight.IndexGenerator     (generator)
import qualified Gimlight.Localization.Texts as T
import           Gimlight.SetUp.CellMap      (dummyTileFile, initTileCollection)
import           Linear.V2                   (V2 (V2))
import           Test.Hspec                  (Spec, it, shouldBe)

spec :: Spec
spec = do
    testMoveSucceed
    testTriedToMoveToUnwalkablePlace
    testTriedToMoveWhereActorExists

testMoveSucceed :: Spec
testMoveSucceed =
    it "succeeds to move if no actor is on the destination and the destination is walkable" $
    resultWhenMoveOffsetTo moveTo `shouldBe` succeed moveTo
  where
    moveTo = V2 1 1

testTriedToMoveToUnwalkablePlace :: Spec
testTriedToMoveToUnwalkablePlace =
    it "fails to move because the destination is not walkable." $
    resultWhenMoveOffsetTo (V2 0 1) `shouldBe` failed

testTriedToMoveWhereActorExists :: Spec
testTriedToMoveWhereActorExists =
    it "fails to move because there is an actor on the destination." $
    resultWhenMoveOffsetTo (V2 1 0) `shouldBe` failed

succeed :: V2 Int -> ActionResultWithLog
succeed offset = writer (result, [])
  where
    result =
        ActionResult {status = Ok, newCellMap = cellMapWithPlayer, killed = []}
    cellMapWithPlayer =
        fromRight' $
        flip execStateT testMap $ do
            a <- removeActorAt (V2 0 0)
            locateActorAt initTileCollection a (V2 0 0 + offset)

failed :: ActionResultWithLog
failed = writer (result, l)
  where
    result = failedResult testMap
    l = [T.youCannotMoveThere]

resultWhenMoveOffsetTo :: V2 Int -> ActionResultWithLog
resultWhenMoveOffsetTo offset =
    moveAction offset (V2 0 0) initTileCollection testMap

testMap :: CellMap
testMap =
    fromRight' $
    flip execStateT cm $ do
        locateActorAt initTileCollection o1 (V2 0 0)
        locateActorAt initTileCollection o2 (V2 1 0) -- TODO: Change `p` as two actors have the same ID.
  where
    cm =
        cellMap $
        array
            (V2 0 0, V2 1 1)
            [ (V2 0 0, TileIdLayer Nothing Nothing)
            , (V2 1 0, TileIdLayer Nothing Nothing)
            , (V2 0 1, TileIdLayer (Just (dummyTileFile, 1)) Nothing)
            , (V2 1 1, TileIdLayer Nothing Nothing)
            ]
    (o1, o2) = flip evalState generator $ (,) <$> orc <*> orc
