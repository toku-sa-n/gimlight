module Action
    ( Action
    , ActionResultWithLog
    , ActionStatus(..)
    ) where

import           Actor                      (Actor)
import           Control.Monad.Trans.Writer (Writer)
import           Dungeon                    (Dungeon)
import           Item.Book                  (Book)
import           Log                        (MessageLog)

data ActionStatus
    = Ok
    | ReadingStarted Book
    | Failed

type Action = Actor -> Dungeon -> ActionResultWithLog

type ActionResult = (ActionStatus, Dungeon)

type ActionResultWithLog = Writer MessageLog ActionResult
