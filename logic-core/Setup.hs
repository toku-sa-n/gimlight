{-# OPTIONS_GHC -Weverything -Werror -Wno-safe -Wno-unsafe -Wno-missing-safe-haskell-mode #-}

module Main
  ( main
  ) where

import           Control.Monad       (unless)
import           Distribution.Simple (UserHooks (buildHook, confHook),
                                      defaultMainWithHooks, simpleUserHooks)
import           Prelude             (IO, writeFile, ($))
import           System.Directory    (doesFileExist)
import           System.Process      (callCommand)

main :: IO ()
main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks =
  simpleUserHooks
    { confHook =
        \pkg flags -> do
          createDummyFile
          callCommand "coq_makefile -f _CoqProject **/*.v -o Makefile"
          confHook simpleUserHooks pkg flags
    , buildHook =
        \pkg lbi hk flags -> do
          callCommand "make -j"
          buildHook simpleUserHooks pkg lbi hk flags
    -- We do not implement `cleanHook` because current Cabal has a bug and does
    -- not call it.
    -- See https://github.com/haskell/cabal/issues/6112.
    }

-- We need to create a dummy file to register the module with the `.cabal` file.
createDummyFile :: IO ()
createDummyFile = do
  fileExists <- doesFileExist "src/GimlightLogicCore.hs"
  unless fileExists $ writeFile "src/GimlightLogicCore.hs" ""
