module Gimlight.Action.MoveOneSquare
    ( moveOneSquareAction
    ) where

import           Control.Monad.State         (execStateT)
import           Control.Monad.Writer        (tell)
import           Gimlight.Action             (Action,
                                              ActionResult (ActionResult),
                                              ActionStatus (Failed, Ok))
import           Gimlight.Actor              (changeDirection)
import           Gimlight.Direction          (Direction, toUnitVector)
import           Gimlight.Dungeon.Map.Cell   (Error (ActorAlreadyExists, OutOfRange, TileIsNotWalkable),
                                              locateActorAt, removeActorAt)
import qualified Gimlight.Localization.Texts as T

moveOneSquareAction :: Direction -> Action
moveOneSquareAction dir position tiles cm =
    case result of
        Right x                     -> return $ ActionResult Ok x []
        Left (ActorAlreadyExists _) -> cannotMove
        Left TileIsNotWalkable      -> cannotMove
        Left OutOfRange             -> cannotMove
        _                           -> error "Unreachable."
  where
    result =
        flip execStateT cm $ do
            a <- removeActorAt position
            let facingUpdated = changeDirection dir a
            locateActorAt tiles dst facingUpdated
    dst = position + toUnitVector dir
    cannotMove = do
        tell [T.youCannotMoveThere]
        return $ ActionResult Failed cm []
