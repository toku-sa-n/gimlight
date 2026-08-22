type direction = Left | Right | Up | Down

type t =
  | Move of direction
  | Quit

let to_logic_direction = function
  | Left -> Gimlight_logic.Direction.Coq_left
  | Right -> Gimlight_logic.Direction.Coq_right
  | Up -> Gimlight_logic.Direction.Coq_up
  | Down -> Gimlight_logic.Direction.Coq_down
