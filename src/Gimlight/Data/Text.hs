module Gimlight.Data.Text
    ( FilePath
    , readFile
    , showt
    ) where

import           Data.Text    (Text, pack, unpack)
import qualified Data.Text.IO as TIO
import           Prelude      (IO, Show (show), (.))

-- We should use types provided from the `path` library
-- instead of this `FilePath` alias. However, it isn't
-- easy to use them because source code files, map files,
-- and tile files contain Windows and Unix style paths,
-- while the library supports only one type of these
-- two types at a time.
type FilePath = Text

readFile :: FilePath -> IO Text
readFile = TIO.readFile . unpack

showt :: (Show a) => a -> Text
showt = pack . show
