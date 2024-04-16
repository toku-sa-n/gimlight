{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE Safe                     #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gimlight.GameModel
  ( GameModel
  , initGameModel
  ) where

import           Data.Kind (Type)
import           Prelude   (Eq)

type GameModel :: Type

data GameModel =
  GameModel
  deriving stock (Eq)

initGameModel :: GameModel
initGameModel = GameModel
