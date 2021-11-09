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

type ActionResultWithLog = Writer MessageLog (ActionStatus, Dungeon)
