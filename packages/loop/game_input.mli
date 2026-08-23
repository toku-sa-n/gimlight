type direction = Left | Right | Up | Down

type t =
  | Move of direction
  | Quit

val to_logic_direction : direction -> Gimlight_logic.Direction.t
val to_logic_input : t -> Gimlight_logic.GameInput.t
