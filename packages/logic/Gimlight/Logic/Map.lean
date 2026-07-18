module

public import Gimlight.Logic.Dimensions

namespace Gimlight

public structure Map where private mk ::
  public dimensions : Dimensions
  private positiveDimensions : 0 < dimensions.width ∧ 0 < dimensions.height
deriving DecidableEq, Repr

public def defaultMap : Map :=
  .mk { width := 20, height := 10 } (by decide)

public theorem Map.dimensionsPositive (map : Map) :
    0 < map.dimensions.width ∧ 0 < map.dimensions.height :=
  map.positiveDimensions

end Gimlight
