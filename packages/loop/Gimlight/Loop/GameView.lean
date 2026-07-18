module

import Std

namespace Gimlight

public structure GameView where
  width : Nat
  height : Nat
  playerX : Nat
  playerY : Nat
deriving DecidableEq, Repr

end Gimlight
