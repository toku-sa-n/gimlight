module Gimlight.System.Path
    ( canonicalizeToUnixStyleRelativePath
    ) where

import           Control.Monad    ((>=>))
import           Gimlight.Prelude
import           System.Directory (canonicalizePath,
                                   makeRelativeToCurrentDirectory)
import           System.Info      (os)

-- `canonicalizePath` makes a Windows-style path on Windows. However, all
-- paths written in the source codes are Unix-style. It causes some
-- problems. For example, a key-not-found error will happen if we try to
-- access a map value using a Windows-style path, but the map uses
-- Unix-style paths as the keys. This function makes a path to a Unix-style
-- relative path to prevent it.
canonicalizeToUnixStyleRelativePath :: FilePath -> IO FilePath
canonicalizeToUnixStyleRelativePath =
    case os of
        "mingw32" -> fmap replaceBackSlash . toRel
        _         -> toRel
  where
    toRel = canonicalizePath >=> makeRelativeToCurrentDirectory

replaceBackSlash :: String -> String
replaceBackSlash []        = []
replaceBackSlash ('\\':xs) = '/' : replaceBackSlash xs
replaceBackSlash (x:xs)    = x : replaceBackSlash xs
