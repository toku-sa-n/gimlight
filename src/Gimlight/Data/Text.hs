module Gimlight.Data.Text
    ( showt
    ) where

import           Data.Text (Text, pack)
import           Prelude   (Show (show), (.))

showt :: (Show a) => a -> Text
showt = pack . show
