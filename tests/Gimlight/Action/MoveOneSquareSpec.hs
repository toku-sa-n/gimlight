module Gimlight.Action.MoveOneSquareSpec
    ( spec
    ) where

import           Control.Lens                  (set)
import           Control.Monad.State           (evalState, execStateT)
import           Control.Monad.Writer          (writer)
import           Data.Either.Combinators       (fromRight')
import           Data.OpenUnion                (liftUnion)
import           Gimlight.Action               (ActionResultWithLog)
import           Gimlight.Action.MoveOneSquare (moveOneSquareAction)
import           Gimlight.ActionSpec           (failedResult, okResult)
import           Gimlight.Actor                (facing)
import           Gimlight.Actor.Monsters       (orc)
import           Gimlight.Coord                (Coord)
import           Gimlight.Direction            (Direction (East, South, SouthEast, West),
                                                toUnitVector)
import           Gimlight.Dungeon.Map.Cell     (CellMap, locateActorAt,
                                                removeActorAt, tileIdLayer)
import           Gimlight.Dungeon.Map.CellSpec (emptyCellMap, locateItemsActors)
import           Gimlight.Dungeon.Map.TileSpec (mockTileCollection, unwalkable)
import           Gimlight.IndexGenerator       (generator)
import qualified Gimlight.Localization.Texts   as T
import           Gimlight.Prelude
import           Linear.V2                     (V2 (V2))
import           Test.Hspec                    (Expectation, Spec, it, shouldBe)

spec :: Spec
spec = do
    testMoveSucceed
    testTriedToMoveToUnwalkablePlace
    testTriedToMoveWhereActorExists
    testTriedToMoveOutsideOfMap

testMoveSucceed :: Spec
testMoveSucceed =
    it "succeeds to move if no actor is on the destination and the destination is walkable" $
    resultWhenMoveTo moveTo `shouldBe`
    succeed moveTo
  where
    moveTo = SouthEast

testTriedToMoveToUnwalkablePlace :: Spec
testTriedToMoveToUnwalkablePlace =
    it "fails to move because the destination is not walkable." $
    failMovingTo South

testTriedToMoveWhereActorExists :: Spec
testTriedToMoveWhereActorExists =
    it "fails to move because there is an actor on the destination." $
    failMovingTo East

testTriedToMoveOutsideOfMap :: Spec
testTriedToMoveOutsideOfMap =
    it "fails to move because the actor tried to move to the outside of the map." $
    failMovingTo West

succeed :: Direction -> ActionResultWithLog
succeed dir = writer (okResult cellMapWithPlayer, [])
  where
    cellMapWithPlayer =
        fromRight' $ flip execStateT testMap $ removeActorAt startPos >>=
        locateActorAt mockTileCollection dst .
        set facing dir
    dst = startPos + toUnitVector dir

failMovingTo :: Direction -> Expectation
failMovingTo dir = resultWhenMoveTo dir `shouldBe` failed

failed :: ActionResultWithLog
failed = writer (failedResult testMap, [T.youCannotMoveThere])

resultWhenMoveTo :: Direction -> ActionResultWithLog
resultWhenMoveTo d = moveOneSquareAction d (V2 0 0) mockTileCollection testMap

testMap :: CellMap
testMap =
    locateItemsActors [(startPos, liftUnion o1), (V2 1 0, liftUnion o2)] cm
  where
    cm =
        emptyCellMap (V2 2 2) & ix (V2 0 1) . tileIdLayer .~
        [Just unwalkable, Nothing]
    (o1, o2) = flip evalState generator $ (,) <$> orc <*> orc

startPos :: Coord
startPos = V2 0 0
