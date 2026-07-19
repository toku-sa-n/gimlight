module

public import Gimlight.Logic.Direction

namespace Gimlight

public structure Position where
  x : Nat
  y : Nat
deriving DecidableEq, Repr

@[expose] public def Position.step (direction : Direction) (position : Position) : Option Position :=
  match direction with
  | .left => if position.x = 0 then none else some { position with x := position.x - 1 }
  | .right => some { position with x := position.x + 1 }
  | .up => if position.y = 0 then none else some { position with y := position.y - 1 }
  | .down => some { position with y := position.y + 1 }

end Gimlight
