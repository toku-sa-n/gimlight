module Core where

import qualified Prelude

type Game_model =
  Prelude.Int
  -- singleton inductive, whose constructor was mk_game_model
  
init_game_model :: Game_model
init_game_model =
  0

increment :: Game_model -> Game_model
increment gm =
  (Prelude.+) gm (Prelude.succ 0)

get_count :: Game_model -> Prelude.Int
get_count gm =
  gm

