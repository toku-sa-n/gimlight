let () =
  let rendered = ref [] in
  let inputs =
    ref [ Game_input.Move Game_input.Left; Game_input.Quit ]
  in
  let host : (module Game_loop.Host) =
    (module struct
      let render view = rendered := view :: !rendered

      let read_input () =
        match !inputs with
        | input :: remaining ->
            inputs := remaining;
            input
        | [] -> failwith "loop test consumed too many inputs"
    end)
  in
  Game_loop.run host;
  match List.rev !rendered with
  | [ initial; moved ] ->
      if (initial.Game_view.width, initial.Game_view.height) <> (20, 10) then
        failwith "loop should convert the initial dimensions";
      if (initial.Game_view.player_x, initial.Game_view.player_y) <> (10, 5) then
        failwith "loop should render the initial state";
      if (moved.Game_view.player_x, moved.Game_view.player_y) <> (9, 5) then
        failwith "loop should apply the requested movement"
  | _ -> failwith "loop should render once before every input"
