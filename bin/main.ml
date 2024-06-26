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
  match Event.get ev Event.typ with
  | x when x == Event.key_down -> (
      match Event.get ev Event.keyboard_keycode with
      | x when x == K.return -> action ()
      | _ -> ())
  | _ -> ()

let widgets = [ label ]

let connections =
  List.map
    (fun w ->
      Widget.connect_main w w
        (fun _ _ -> on_user_event)
        [ Trigger.key_up; Trigger.key_down ])
    widgets

let layout = Layout.flat_of_w ~name:"Counter App" widgets
let () = Bogue.of_layout ~on_user_event layout ~connections |> Bogue.run
