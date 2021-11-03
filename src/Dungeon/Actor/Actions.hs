module Dungeon.Actor.Actions
    ( Action
    , ActionResult
    , ActionStatus(..)
    ) where

import           Control.Monad.Trans.Maybe  (MaybeT)
import           Control.Monad.Trans.Writer (Writer)
import           Dungeon                    (Dungeon)
import           Dungeon.Actor              (Actor)
import           Dungeon.Item.Book          (Book)
import           Log                        (MessageLog)

data ActionStatus = Ok
                  | ReadingStarted Book

type Action = Actor -> Dungeon -> ActionResult

type ActionResult = MaybeT (Writer MessageLog) (ActionStatus, Dungeon)
