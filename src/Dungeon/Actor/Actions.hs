module Dungeon.Actor.Actions
    ( Action
    , ActionResult
    , ActionStatus(..)
    ) where

import           Control.Monad.Trans.Maybe  (MaybeT)
import           Control.Monad.Trans.Writer (Writer)
import           Dungeon                    (Dungeon)
import           Dungeon.Actor              (Actor)
import           Dungeon.Item.Book          (BookHandler)
import           Log                        (MessageLog)

data ActionStatus = Ok
                  | ReadingStarted BookHandler

type Action = Actor -> Dungeon -> ActionResult

type ActionResult = MaybeT (Writer MessageLog) (ActionStatus, Dungeon)
