module Gimlight.ActionSpec
    ( okResult
    , failedResult
    ) where

import           Gimlight.Action           (ActionResult (ActionResult, killed, newCellMap, status),
                                            ActionStatus (Failed, Ok))
import           Gimlight.Dungeon.Map.Cell (CellMap)

okResult :: CellMap -> ActionResult
okResult cm = ActionResult {status = Ok, newCellMap = cm, killed = []}

failedResult :: CellMap -> ActionResult
failedResult cm = ActionResult {status = Failed, newCellMap = cm, killed = []}
