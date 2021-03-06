module Gimlight.Data.Either
    ( expectRight
    ) where

import           GHC.Stack        (HasCallStack)
import           Gimlight.Prelude

expectRight :: (Show b, HasCallStack) => Text -> Either b a -> a
expectRight _ (Right x)  = x
expectRight msg (Left x) = error $ msg <> ": " <> showt x
