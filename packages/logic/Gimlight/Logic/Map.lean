module

import Std

namespace Gimlight

public structure Map where private mk ::
  public width : Nat
  public height : Nat
  private positiveDimensions : 0 < width ∧ 0 < height
deriving DecidableEq, Repr

public def defaultMap : Map :=
  .mk 20 10 (by decide)

public theorem Map.dimensionsPositive (map : Map) :
    0 < map.width ∧ 0 < map.height :=
  map.positiveDimensions

end Gimlight
