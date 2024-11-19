type t = {
  get : unit -> Entity.game_model;
  save : Entity.game_model -> unit;
  init : unit -> unit;
}

val make : t
