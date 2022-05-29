module Gimlight.System.Path
    ( canonicalizeToUnixStyleRelativePath
    , dropFileName
    , takeExtension
    , (</>)
    ) where

import           Control.Monad    ((>=>))
import           Data.Text        (pack, replace, unpack)
import           Gimlight.Prelude
import           System.Directory (canonicalizePath,
                                   makeRelativeToCurrentDirectory)
import qualified System.FilePath  as FilePath
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
    toRel =
        canonicalizePath . unpack >=> fmap pack . makeRelativeToCurrentDirectory

dropFileName :: FilePath -> FilePath
dropFileName = pack . FilePath.dropFileName . unpack

takeExtension :: FilePath -> FilePath
takeExtension = pack . FilePath.takeExtension . unpack

(</>) :: Text -> Text -> Text
x </> y = pack $ unpack x FilePath.</> unpack y

replaceBackSlash :: Text -> Text
replaceBackSlash = replace "\\" "/"
