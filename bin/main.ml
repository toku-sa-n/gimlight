open Bogue

let model = ref Gimlight.Game_model.init_game_model

let increment () = model := Gimlight.Game_model.increment !model

let label_message n = "You pressed the Enter key " ^ string_of_int n ^ " times"

let update c m =
  m |> Gimlight.Game_model.get_count |> Z.to_int |> label_message
  |> Widget.set_text c

let () =
  let label = Widget.label "You pressed the Enter key 0 times" in
  let action _ = increment () ; update label !model in
  let button = Widget.button ~action "Press me" in
  Layout.flat_of_w ~name:"Counter App" [label; button]
  |> Bogue.of_layout |> Bogue.run
