module

import Std

namespace Gimlight

public inductive Direction where
  | left
  | right
  | up
  | down
deriving DecidableEq, Repr

end Gimlight
