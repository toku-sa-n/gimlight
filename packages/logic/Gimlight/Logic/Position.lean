module

public import Gimlight.Logic.Direction
public import Gimlight.Logic.Map.Core

namespace Gimlight

public def Position.inBounds (map : Map) (position : Position) : Prop :=
  position.x < map.dimensions.width ∧ position.y < map.dimensions.height

end Gimlight
