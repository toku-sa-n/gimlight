let fail message = failwith ("logic test failed: " ^ message)
let check condition message = if not condition then fail message

let coordinate state =
  let map = Gimlight_logic.GameState.map state in
  let player = Gimlight_logic.GameState.player state in
  ( Gimlight_logic.Position.x_value map player,
    Gimlight_logic.Position.y_value map player )

let step input state =
  match Gimlight_logic.GameStep.step input state with
  | Gimlight_logic.GameStep.Continue next_state -> next_state
  | Gimlight_logic.GameStep.Quit -> fail "a movement input should continue"

let move_n direction count state =
  let rec loop remaining state =
    if remaining = 0 then state
    else
      loop (remaining - 1)
        (step (Gimlight_logic.GameInput.Move direction) state)
  in
  loop count state

let () =
  let state = Gimlight_logic.GameState.initialState in
  check (coordinate state = (10, 5)) "initial state should be centered";
  check
    (match Gimlight_logic.GameStep.step Gimlight_logic.GameInput.Quit state with
    | Gimlight_logic.GameStep.Quit -> true
    | Gimlight_logic.GameStep.Continue _ -> false)
    "quit input should stop the game";
  let left = move_n Gimlight_logic.Direction.Rocq_left 20 state in
  check (coordinate left = (0, 5)) "left movement should stop at the boundary";
  let right = move_n Gimlight_logic.Direction.Rocq_right 30 state in
  check
    (coordinate right = (19, 5))
    "right movement should stop at the boundary";
  let up = move_n Gimlight_logic.Direction.Rocq_up 20 state in
  check (coordinate up = (10, 0)) "up movement should stop at the boundary";
  let down = move_n Gimlight_logic.Direction.Rocq_down 20 state in
  check (coordinate down = (10, 9)) "down movement should stop at the boundary"
