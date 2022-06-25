module Gimlight.Action.MoveOneSquare
    ( moveOneSquareAction
    ) where

import           Control.Lens                (set)
import           Control.Monad.State         (execStateT)
import           Control.Monad.Writer        (tell)
import           Gimlight.Action             (Action,
                                              ActionResult (ActionResult),
                                              ActionStatus (Failed, Ok))
import           Gimlight.Actor              (facing)
import           Gimlight.Direction          (Direction, toUnitVector)
import           Gimlight.Dungeon.Map.Cell   (Error (ActorAlreadyExists, OutOfRange, TileIsNotWalkable),
                                              locateActorAt, removeActorAt)
import qualified Gimlight.Localization.Texts as T
import           Gimlight.Prelude

moveOneSquareAction :: Direction -> Action
moveOneSquareAction dir position tiles cm =
    case result of
        Right x                     -> return $ ActionResult Ok x []
        Left (ActorAlreadyExists _) -> cannotMove
        Left TileIsNotWalkable      -> cannotMove
        Left OutOfRange             -> cannotMove
        _                           -> error "Unreachable."
  where
    result = execStateT moveState cm
    moveState =
        removeActorAt position >>= locateActorAt tiles dst . set facing dir
    dst = position + toUnitVector dir
    cannotMove = do
        tell [T.youCannotMoveThere]
        return $ ActionResult Failed cm []
