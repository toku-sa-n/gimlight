module

import Std

namespace Gimlight

public inductive InputDirection where
  | left
  | right
  | up
  | down
deriving DecidableEq, Repr

public inductive GameInput where
  | move : InputDirection -> GameInput
  | quit : GameInput

end Gimlight
