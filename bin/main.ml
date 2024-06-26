let model =
  Gimlight.Game_model.init_game_model |> Gimlight.Game_model.increment
  |> Gimlight.Game_model.increment |> Gimlight.Game_model.get_count

let () = print_endline (Z.to_string model)
