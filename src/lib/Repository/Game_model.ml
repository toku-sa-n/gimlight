type t = {
  get : unit -> Entity.game_model;
  save : Entity.game_model -> unit;
  init : unit -> unit;
}

let game_model = ref Entity.init_game_model

let make =
  {
    get = (fun () -> !game_model);
    save = (fun x -> game_model := x);
    init = (fun () -> game_model := Entity.init_game_model);
  }
