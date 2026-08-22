module type Host = sig
  val render : Game_view.t -> unit
  val read_input : unit -> Game_input.t
end

let rec run_from render read_input state =
  render (Game_view.of_state state);
  match read_input () with
  | Game_input.Quit -> ()
  | Game_input.Move direction ->
      let state =
        Gimlight_logic.GameState.move
          (Game_input.to_logic_direction direction) state
      in
      run_from render read_input state

let run (module H : Host) =
  run_from H.render H.read_input Gimlight_logic.GameState.initialState
