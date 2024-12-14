type t
type output = bool array array

val make : Repository.Map.t -> t
val execute : t -> output
