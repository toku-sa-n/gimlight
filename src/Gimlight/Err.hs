module Gimlight.Err
    ( error
    ) where

import           Data.Function ((.))
import           Data.Text     (Text, unpack)
import           GHC.Stack     (HasCallStack)
import qualified Prelude

error :: HasCallStack => Text -> a
error = Prelude.error . unpack
