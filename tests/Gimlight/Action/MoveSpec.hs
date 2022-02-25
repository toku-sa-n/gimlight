module Gimlight.Action.MoveSpec
    ( spec
    ) where

import           Control.Lens                  (ix, (&), (.~))
import           Control.Monad.State           (evalState, execStateT)
import           Control.Monad.Writer          (writer)
import           Data.Either.Combinators       (fromRight')
import           Gimlight.Action               (ActionResultWithLog)
import           Gimlight.Action.Move          (moveAction)
import           Gimlight.ActionSpec           (failedResult, okResult)
import           Gimlight.Actor.Monsters       (orc)
import           Gimlight.Dungeon.Map.Cell     (CellMap,
                                                TileIdLayer (TileIdLayer),
                                                locateActorAt, removeActorAt,
                                                tileIdLayer)
import           Gimlight.Dungeon.Map.CellSpec (emptyCellMap)
import           Gimlight.IndexGenerator       (generator)
import qualified Gimlight.Localization.Texts   as T
import           Gimlight.SetUp.CellMap        (dummyTileFile,
                                                mockTileCollection)
import           Linear.V2                     (V2 (V2))
import           Test.Hspec                    (Spec, it, shouldBe)

spec :: Spec
spec = do
    testMoveSucceed
    testTriedToMoveToUnwalkablePlace
    testTriedToMoveWhereActorExists

testMoveSucceed :: Spec
testMoveSucceed =
    it "succeeds to move if no actor is on the destination and the destination is walkable" $
    resultWhenMoveOffsetTo moveTo `shouldBe`
    succeed moveTo
  where
    moveTo = V2 1 1

testTriedToMoveToUnwalkablePlace :: Spec
testTriedToMoveToUnwalkablePlace =
    it "fails to move because the destination is not walkable." $
    resultWhenMoveOffsetTo (V2 0 1) `shouldBe`
    failed

testTriedToMoveWhereActorExists :: Spec
testTriedToMoveWhereActorExists =
    it "fails to move because there is an actor on the destination." $
    resultWhenMoveOffsetTo (V2 1 0) `shouldBe`
    failed

succeed :: V2 Int -> ActionResultWithLog
succeed offset = writer (okResult cellMapWithPlayer, [])
  where
    cellMapWithPlayer =
        fromRight' $ flip execStateT testMap $ do
            a <- removeActorAt (V2 0 0)
            locateActorAt mockTileCollection a (V2 0 0 + offset)

failed :: ActionResultWithLog
failed = writer (failedResult testMap, [T.youCannotMoveThere])

resultWhenMoveOffsetTo :: V2 Int -> ActionResultWithLog
resultWhenMoveOffsetTo offset =
    moveAction offset (V2 0 0) mockTileCollection testMap

testMap :: CellMap
testMap =
    fromRight' $ flip execStateT cm $ do
        locateActorAt mockTileCollection o1 (V2 0 0)
        locateActorAt mockTileCollection o2 (V2 1 0)
  where
    cm =
        emptyCellMap (V2 2 2) & ix (V2 0 1) . tileIdLayer .~
        TileIdLayer (Just (dummyTileFile, 1)) Nothing
    (o1, o2) = flip evalState generator $ (,) <$> orc <*> orc
