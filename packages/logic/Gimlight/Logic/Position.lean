module

public import Gimlight.Logic.Direction
public import Gimlight.Logic.Map.Basic

namespace Gimlight

public def Position.inBounds (map : Map) (position : Position) : Prop :=
  position.x < map.dimensions.width ∧ position.y < map.dimensions.height

public def Position.step (direction : Direction) (position : Position) : Option Position :=
  match direction with
  | .left => if position.x = 0 then none else some { position with x := position.x - 1 }
  | .right => some { position with x := position.x + 1 }
  | .up => if position.y = 0 then none else some { position with y := position.y - 1 }
  | .down => some { position with y := position.y + 1 }

end Gimlight
