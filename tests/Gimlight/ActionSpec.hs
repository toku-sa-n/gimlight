module Gimlight.ActionSpec
    ( failedResult
    ) where

import           Gimlight.Action           (ActionResult (ActionResult, killed, newCellMap, status),
                                            ActionStatus (Failed))
import           Gimlight.Dungeon.Map.Cell (CellMap)

failedResult :: CellMap -> ActionResult
failedResult cm = ActionResult {status = Failed, newCellMap = cm, killed = []}
