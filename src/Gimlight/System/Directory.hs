module Gimlight.System.Directory
    ( getFilesRecursive
    ) where

import           Data.Text                  (pack, unpack)
import           Gimlight.Prelude
import qualified System.Directory.Recursive as D

getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive = fmap (fmap pack) . D.getFilesRecursive . unpack
