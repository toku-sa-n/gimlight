module Gimlight.Data.Maybe
    ( expectJust
    ) where

import           GHC.Stack (HasCallStack)

expectJust :: HasCallStack => String -> Maybe a -> a
expectJust _ (Just x) = x
expectJust s Nothing  = error s
