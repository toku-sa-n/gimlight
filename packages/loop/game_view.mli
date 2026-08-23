type t = { width : int; height : int; player_x : int; player_y : int }

val of_state : Gimlight_logic.GameState.t -> t
