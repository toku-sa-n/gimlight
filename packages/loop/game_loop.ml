module type Host = sig
  val render : Game_view.t -> unit
  val read_input : unit -> Game_input.t
end

let rec run_from render read_input state =
  render (Game_view.of_state state);
  match read_input () with
  | input -> (
      match
        Gimlight_logic.GameStep.step (Game_input.to_logic_input input) state
      with
      | Gimlight_logic.GameStep.Continue next_state ->
          run_from render read_input next_state
      | Gimlight_logic.GameStep.Quit -> ())

let run (module H : Host) =
  run_from H.render H.read_input Gimlight_logic.GameState.initialState
