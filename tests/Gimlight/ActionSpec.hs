module Gimlight.ActionSpec
    ( okResult
    , failedResult
    , readingResult
    ) where

import           Gimlight.Action           (ActionResult (ActionResult, killed, newCellMap, status),
                                            ActionStatus (Failed, Ok, ReadingStarted))
import           Gimlight.Dungeon.Map.Cell (CellMap)
import           Gimlight.Item.Book        (Book)

okResult :: CellMap -> ActionResult
okResult cm = ActionResult {status = Ok, newCellMap = cm, killed = []}

failedResult :: CellMap -> ActionResult
failedResult cm = ActionResult {status = Failed, newCellMap = cm, killed = []}

readingResult :: Book -> CellMap -> ActionResult
readingResult book cm =
    ActionResult {status = ReadingStarted book, newCellMap = cm, killed = []}
