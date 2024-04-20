module GimlightLogicCore where

import qualified Prelude

type GameModel =
  Prelude.Int
  -- singleton inductive, whose constructor was MkGameModel
  
init_game_model :: GameModel
init_game_model =
  0

increment :: GameModel -> GameModel
increment m =
  (Prelude.+) m (Prelude.succ 0)

get_count :: GameModel -> Prelude.Int
get_count m =
  m

