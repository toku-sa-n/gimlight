module Gimlight.Codec.Picture
    ( readImage
    ) where

import           Codec.Picture           (DynamicImage)
import qualified Codec.Picture           as P
import           Data.Either.Combinators (mapLeft)
import           Data.Text               (pack, unpack)
import           Gimlight.Prelude

readImage :: Text -> IO (Either Text DynamicImage)
readImage = fmap (mapLeft pack) . P.readImage . unpack
