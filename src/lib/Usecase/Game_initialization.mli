type t
type output = { initial_map : bool array array }

val make : Repository.Map.t -> t
val execute : t -> output
