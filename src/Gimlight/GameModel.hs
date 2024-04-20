{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE Safe                     #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gimlight.GameModel
  ( GameModel
  , initGameModel
  , increment
  , getCount
  ) where

import           Data.Kind (Type)
import           Prelude   (Eq, Int, (+))

type GameModel :: Type

newtype GameModel =
  GameModel Int
  deriving stock (Eq)

initGameModel :: GameModel
initGameModel = GameModel 0

increment :: GameModel -> GameModel
increment (GameModel n) = GameModel (n + 1)

getCount :: GameModel -> Int
getCount (GameModel n) = n
