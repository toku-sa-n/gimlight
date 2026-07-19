module

import Std

namespace Gimlight

public inductive ViewTile where
  | wall
  | floor
deriving DecidableEq, Repr

public structure GameView where
  width : Nat
  height : Nat
  tiles : Array ViewTile
  playerX : Nat
  playerY : Nat
deriving DecidableEq, Repr

end Gimlight
