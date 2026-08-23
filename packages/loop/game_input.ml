type direction = Left | Right | Up | Down
type t = Move of direction | Quit

let to_logic_direction = function
  | Left -> Gimlight_logic.Direction.Left
  | Right -> Gimlight_logic.Direction.Right
  | Up -> Gimlight_logic.Direction.Up
  | Down -> Gimlight_logic.Direction.Down

let to_logic_input = function
  | Move direction ->
      Gimlight_logic.GameInput.Move (to_logic_direction direction)
  | Quit -> Gimlight_logic.GameInput.Quit
