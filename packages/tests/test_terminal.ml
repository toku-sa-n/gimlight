let () =
  let view : Game_view.t =
    { Game_view.width = 3; height = 2; player_x = 1; player_y = 0 }
  in
  let expected = ".@.\r\n...\r\n\r\nh/j/k/l, arrow keys: move   q: quit" in
  if Terminal.render_text view <> expected then
    failwith "terminal rendering should preserve the ANSI-era layout"
