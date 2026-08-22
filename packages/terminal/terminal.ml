let escape = Char.chr 27

let render_text view =
  let buffer = Buffer.create 256 in
  for y = 0 to view.Game_view.height - 1 do
    for x = 0 to view.Game_view.width - 1 do
      let tile =
        if x = view.Game_view.player_x && y = view.Game_view.player_y then '@'
        else '.'
      in
      Buffer.add_char buffer tile
    done;
    Buffer.add_string buffer "\r\n"
  done;
  Buffer.add_string buffer "\r\n";
  Buffer.add_string buffer "h/j/k/l, arrow keys: move   q: quit";
  Buffer.contents buffer

let render view =
  output_string stdout "\027[2J\027[H";
  output_string stdout (render_text view);
  flush stdout

type session = {
  original : Unix.terminal_io;
  mutable restored : bool;
}

let enter_raw_mode () =
  let original = Unix.tcgetattr Unix.stdin in
  let raw =
    {
      original with
      Unix.c_icanon = false;
      Unix.c_echo = false;
      Unix.c_vmin = 1;
      Unix.c_vtime = 0;
    }
  in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW raw;
  { original; restored = false }

let restore_terminal session =
  if not session.restored then (
    Unix.tcsetattr Unix.stdin Unix.TCSANOW session.original;
    session.restored <- true)

let read_byte () =
  let buffer = Bytes.create 1 in
  if Unix.read Unix.stdin buffer 0 1 <> 1 then raise End_of_file;
  Bytes.get buffer 0

let read_arrow () =
  match read_byte () with
  | '[' -> (
      match read_byte () with
      | 'A' -> Some Game_input.Up
      | 'B' -> Some Game_input.Down
      | 'C' -> Some Game_input.Right
      | 'D' -> Some Game_input.Left
      | _ -> None)
  | _ -> None

let rec read_input () =
  match read_byte () with
  | 'h' -> Game_input.Move Game_input.Left
  | 'l' -> Game_input.Move Game_input.Right
  | 'k' -> Game_input.Move Game_input.Up
  | 'j' -> Game_input.Move Game_input.Down
  | 'q' -> Game_input.Quit
  | c when Char.code c = Char.code escape -> (
      match read_arrow () with
      | Some direction -> Game_input.Move direction
      | None -> read_input ())
  | _ -> read_input ()

let with_raw_mode action =
  let session = enter_raw_mode () in
  Fun.protect
    ~finally:(fun () -> restore_terminal session)
    action

let run () =
  with_raw_mode (fun () ->
      Game_loop.run
        (module struct
          let render = render
          let read_input = read_input
        end))
