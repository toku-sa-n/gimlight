module Gimlight.Prelude
    ( module Control.Lens
    , module Data.Function
    , module Data.Text
    , module Gimlight.Data.Text
    , module Gimlight.Err
    , module Prelude
    ) where

import           Control.Lens       (Ixed (ix), makeLenses, (%%~), (%~), (+=),
                                     (.=), (.~), (?~), (^.), (^?))
import           Data.Function      ((&))
import           Data.Text          (Text)
import           Gimlight.Data.Text
import           Gimlight.Err
import           Prelude            hiding (error)
