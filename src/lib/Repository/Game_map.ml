type t = { get : unit -> Entity.game_map }

let game_map = ref (Entity.all_wall_map (Z.of_int 80) (Z.of_int 50))
let make = { get = (fun () -> !game_map) }
