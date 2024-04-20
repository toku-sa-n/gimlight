{-# OPTIONS_GHC -Weverything -Werror -Wno-safe -Wno-unsafe -Wno-missing-safe-haskell-mode #-}

module Main
  ( main
  ) where

import           Distribution.Simple (UserHooks (buildHook, cleanHook),
                                      defaultMainWithHooks, simpleUserHooks)
import           Prelude             (IO, undefined)
import           System.Process      (callCommand)

main :: IO ()
main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks =
  simpleUserHooks
    { buildHook =
        \pkg lbi hk flags -> do
          callCommand "coq_makefile -f _CoqProject **/*.v -o Makefile"
          callCommand "make -j"
          buildHook simpleUserHooks pkg lbi hk flags
    -- We do not implement `cleanHook` because current Cabal has a bug and does
    -- not call it.
    -- See https://github.com/haskell/cabal/issues/6112.
    }
