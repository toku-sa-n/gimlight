class game_widget =
  object (self)
    inherit LTerm_widget.t "game"
    val mutable model = { Model.count = 0 }

    method set_model m =
      model <- m;
      self#queue_draw

    method! draw ctx _ =
      let { Model.count } = model in
      let text = Printf.sprintf "You pressed the Enter key %d times." count in
      LTerm_draw.draw_string ctx 0 0 (Zed_string.of_utf8 text)
  end
