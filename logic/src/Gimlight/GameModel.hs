{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE StandaloneKindSignatures, ImportQualifiedPost #-}

module Gimlight.GameModel
  ( GameModel
  , initGameModel
  , increment
  , getCount
  ) where

import           Data.Kind         (Type)
import GimlightLogicCore qualified
import           Prelude           (Int)

type GameModel :: Type

type GameModel = GimlightLogicCore.GameModel

initGameModel :: GameModel
initGameModel = GimlightLogicCore.init_game_model

increment :: GameModel -> GameModel
increment = GimlightLogicCore.increment

getCount :: GameModel -> Int
getCount = GimlightLogicCore.get_count
