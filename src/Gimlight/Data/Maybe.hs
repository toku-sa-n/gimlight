module Gimlight.Data.Maybe
    ( expectJust
    ) where

import           Data.Maybe       (fromMaybe)
import           GHC.Stack        (HasCallStack)
import           Gimlight.Prelude

expectJust :: HasCallStack => Text -> Maybe a -> a
expectJust s = fromMaybe (error s)
