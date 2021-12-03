module Action.Drop
    ( dropAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionStatus (Failed, Ok))
import           Actor                (removeNthItem)
import           Control.Monad.Writer (tell)
import           Data.Maybe           (isJust)
import           Dungeon              (popItemAt, pushActor, pushItem)
import           Item                 (Item, getName)
import qualified Localization.Texts   as T

dropAction :: Int -> Action
dropAction n position e tiles d
    | itemExists = do
        tell [T.itemExists]
        return failedResult
    | otherwise =
        case item of
            Just x -> dropItem x position newActor tiles d
            Nothing -> do
                tell [T.whatToDrop]
                return failedResult
  where
    (item, newActor) = removeNthItem n e
    itemExists = (\(x, _) -> isJust x) $ popItemAt position d
    failedResult = ActionResult Failed (pushActor position e d) []

dropItem :: Item -> Action
dropItem item position actor _ dungeon = do
    tell [T.youDropped $ getName item]
    return $
        ActionResult
            Ok
            (pushActor position actor $ pushItem position item dungeon)
            []
