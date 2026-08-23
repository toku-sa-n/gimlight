type direction = Left | Right | Up | Down
type t = Move of direction | Quit

let to_logic_direction = function
  | Left -> Gimlight_logic.Direction.Rocq_left
  | Right -> Gimlight_logic.Direction.Rocq_right
  | Up -> Gimlight_logic.Direction.Rocq_up
  | Down -> Gimlight_logic.Direction.Rocq_down

let to_logic_input = function
  | Move direction ->
      Gimlight_logic.GameInput.Move (to_logic_direction direction)
  | Quit -> Gimlight_logic.GameInput.Quit
