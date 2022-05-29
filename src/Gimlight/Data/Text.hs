module Gimlight.Data.Text
    ( showt
    ) where

import           Data.Text        (pack)
import           Gimlight.Prelude

showt :: (Show a) => a -> Text
showt = pack . show
