module Gimlight.Action.Wait
    ( waitAction
    ) where

import           Gimlight.Action  (Action, ActionResult (ActionResult),
                                   ActionStatus (Ok))
import           Gimlight.Prelude

waitAction :: Action
waitAction _ _ cm = return $ ActionResult Ok cm []
