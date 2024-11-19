type t = { execute : unit -> unit }

let make (repo : Repository.Game_model.t) =
  { execute = (fun () -> repo.save Entity.init_game_model) }
