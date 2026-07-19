module

import Std

namespace Gimlight

public inductive Direction where
  | left
  | right
  | up
  | down
deriving DecidableEq, Repr

@[expose] public def Direction.opposite : Direction → Direction
  | .left => .right
  | .right => .left
  | .up => .down
  | .down => .up

end Gimlight
