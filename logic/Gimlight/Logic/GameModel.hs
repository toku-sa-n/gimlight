{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE ImportQualifiedPost      #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gimlight.Logic.GameModel
  ( GameModel
  , initGameModel
  , increment
  , getCount
  ) where

import           Data.Kind         (Type)
import           GimlightLogicCore qualified as Core
import           Prelude           (Int)

type GameModel :: Type

type GameModel = Core.Game_model

initGameModel :: GameModel
initGameModel = Core.init_game_model

increment :: GameModel -> GameModel
increment = Core.increment

getCount :: GameModel -> Int
getCount = Core.get_count
