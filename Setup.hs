module Main
  ( main
  ) where

import           Control.Monad       (forM, unless)
import           Distribution.Simple (UserHooks (buildHook, confHook),
                                      defaultMainWithHooks, simpleUserHooks)
import           Prelude             (FilePath, IO, concat, not, pure, unwords,
                                      writeFile, ($), (&&), (++), (==))
import           System.Directory    (doesDirectoryExist, doesFileExist,
                                      listDirectory, pathIsSymbolicLink)
import           System.FilePath     (takeExtension, (</>))
import           System.Process      (callCommand)

main :: IO ()
main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks =
  simpleUserHooks
    { confHook =
        \pkg flags
          -- We manually traverse the directory to find all Coq files instead of
          -- using `**/*.v` as it is shell-specific.
         -> do
          coqFiles <- getAllCoqFiles
          let command =
                "coq_makefile -f _CoqProject " ++
                unwords coqFiles ++ " -o Makefile"
          callCommand command
          confHook simpleUserHooks pkg flags
    , buildHook =
        \pkg lbi hk flags -> do
          callCommand "make -j"
          buildHook simpleUserHooks pkg lbi hk flags
    -- We do not implement `cleanHook` because current Cabal has a bug and does
    -- not call it.
    -- See https://github.com/haskell/cabal/issues/6112.
    }

getAllCoqFiles :: IO [FilePath]
getAllCoqFiles = getAllCoqFiles' "logic-core"
  where
    getAllCoqFiles' :: FilePath -> IO [FilePath]
    getAllCoqFiles' path = do
      files <- listDirectory path
      coqFiles <-
        forM files $ \file -> do
          let fullPath = path </> file
          isDirectory <- doesDirectoryExist fullPath
          isSymbolicLink <- pathIsSymbolicLink fullPath
          -- We do not follow symbolic links for simplicity.
          -- (Consider a symbolic link pointing to a parent directory.)
          if isDirectory && not isSymbolicLink
            then getAllCoqFiles' fullPath
            else if takeExtension file == ".v"
                   then pure [fullPath]
                   else pure []
      pure $ concat coqFiles
