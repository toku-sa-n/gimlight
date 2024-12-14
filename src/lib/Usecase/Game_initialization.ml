module M = Kernel.GameInitializationUsecase

type t = M.t
type output = M.output

let make x = x

(* TODO Assign the correct numbers *)
let execute = M.execute Z.zero Z.zero
