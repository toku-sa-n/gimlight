type t = { execute : unit -> Z.t }

let make (repo : Repository.Game_model.t) =
  {
    execute =
      (fun () ->
        let current = repo.get () in
        let incremented = Entity.increment current in
        let () = repo.save incremented in
        Entity.get_count incremented);
  }
