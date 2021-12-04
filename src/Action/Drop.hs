module Action.Drop
    ( dropAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionResultWithLog,
                                       ActionStatus (Failed, Ok))
import           Actor                (removeNthItem)
import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad.Writer (tell)
import           Dungeon              (cellMap, pushActor)
import           Dungeon.Map.Cell     (locateItemAt)
import           Item                 (Item, getName)
import           Localization         (MultilingualText)
import qualified Localization.Texts   as T

dropAction :: Int -> Action
dropAction n position e _ d =
    case item of
        Just x  -> dropItem x
        Nothing -> failWithReason T.whatToDrop
  where
    (item, newActor) = removeNthItem n e
    failWithReason :: MultilingualText -> ActionResultWithLog
    failWithReason reason = do
        tell [reason]
        return failedResult
    failedResult = ActionResult Failed (pushActor position e d) []
    dropItem :: Item -> ActionResultWithLog
    dropItem item' =
        case locateItemAt item' position (d ^. cellMap) of
            Just newCellMap -> do
                tell [T.youDropped $ getName item']
                return $
                    ActionResult
                        Ok
                        (pushActor position newActor (d & cellMap .~ newCellMap))
                        []
            Nothing -> do
                tell [T.itemExists]
                return $ ActionResult Failed (pushActor position e d) []
