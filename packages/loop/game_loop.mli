module type Host = sig
  val render : Game_view.t -> unit
  val read_input : unit -> Game_input.t
end

val run_from : (Game_view.t -> unit) -> (unit -> Game_input.t) ->
  Gimlight_logic.GameState.t -> unit
val run : (module Host) -> unit
