class game_widget =
  object (self)
    inherit LTerm_widget.t "game"
    val mutable model = { Model.is_wall = [ [ true ]; [ true ] ] }

    method set_model m =
      model <- m;
      self#queue_draw

    method! draw ctx _ =
      let { Model.is_wall } = model in

      List.iteri
        (fun idx row ->
          let text =
            List.fold_left
              (fun acc is_wall ->
                let text = if is_wall then "#" else "*" in
                acc ^ text)
              "" row
          in
          LTerm_draw.draw_string ctx idx 0 (Zed_string.of_utf8 text))
        is_wall
  end
