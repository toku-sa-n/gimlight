type t = { map_repository : Repository.Map.t }
type output = { initial_map : bool array array }

let make map_repository = { map_repository }
let execute { map_repository } = { initial_map = map_repository.get () }
