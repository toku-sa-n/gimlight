class game_widget =
  object (self)
    inherit LTerm_widget.t "game"
    val mutable model = [| [| true |] |]

    method set_model m =
      model <- m;
      self#queue_draw

    method! draw ctx _ =
      let rows = Array.make (Array.length model.(0)) "" in
      Array.iter
        (fun row ->
          Array.iteri
            (fun y cell -> rows.(y) <- (rows.(y) ^ if cell then "#" else " "))
            row)
        model;

      Array.iteri
        (fun idx row ->
          LTerm_draw.draw_string ctx idx 0 (Zed_string.of_utf8 row))
        rows
  end
