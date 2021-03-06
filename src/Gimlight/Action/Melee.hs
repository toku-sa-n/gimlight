module Gimlight.Action.Melee
    ( meleeAction
    ) where

import           Control.Monad.State       (StateT (runStateT))
import           Control.Monad.Writer      (runWriter, tell)
import           Gimlight.Action           (Action, ActionResult (ActionResult),
                                            ActionStatus (Ok))
import           Gimlight.Actor            (facing)
import qualified Gimlight.Actor            as A
import           Gimlight.Direction        (Direction, toUnitVector)
import           Gimlight.Dungeon.Map.Cell (locateActorAt, removeActorAt)
import           Gimlight.Prelude

meleeAction :: Direction -> Action
meleeAction direction srcPosition tc cm =
    case result of
        Right ((l, killed), newMap) -> do
            tell l
            return $ ActionResult Ok newMap killed
        _ -> error "Unreachable"
  where
    result = runStateT meleeState cm
    meleeState = do
        attacker <- removeActorAt srcPosition
        defender <- removeActorAt dstPosition
        let ((newAttacker, newDefender), l') =
                runWriter $ A.attackFromTo attacker defender
        locateActorAt tc srcPosition (newAttacker & facing .~ direction)
        case newDefender of
            Just x -> do
                locateActorAt tc dstPosition x
                return (l', [])
            Nothing -> return (l', [defender])
    dstPosition = srcPosition + toUnitVector direction
