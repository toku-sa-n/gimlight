let model =
  Gimlight.init_game_model |> Gimlight.increment |> Gimlight.increment
  |> Gimlight.get_count

let () = print_endline (Big_int.string_of_big_int model)
