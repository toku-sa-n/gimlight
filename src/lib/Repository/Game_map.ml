type t = { get : unit -> Entity.Map.t }

let game_map = ref Entity.Map.initial_map
let make = { get = (fun () -> !game_map) }
