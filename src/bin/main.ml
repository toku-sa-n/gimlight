open Bogue

let model = ref Gimlight.Game_model.init_game_model
let increment () = model := Gimlight.Game_model.increment !model
let label_message n = "You pressed the Enter key " ^ string_of_int n ^ " times"

let update c m =
  m |> Gimlight.Game_model.get_count |> Z.to_int |> label_message
  |> Widget.set_text c

let label = Widget.label (label_message 0)

let action _ =
  increment ();
  update label !model

let on_user_event ev =
  let open Tsdl.Sdl in
  if
    Event.get ev Event.typ == Event.key_down
    && Event.get ev Event.keyboard_keycode == K.return
  then action ()

let connections =
  [
    Widget.connect_main label label
      (fun _ _ -> on_user_event)
      [ Trigger.key_up; Trigger.key_down ];
  ]

let layout = Layout.flat_of_w ~name:"Counter App" [ label ]
let () = Bogue.of_layout ~on_user_event layout ~connections |> Bogue.run
