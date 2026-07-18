module

public import Gimlight.Logic.Direction
public import Gimlight.Logic.Map

namespace Gimlight

public structure Position where private mk ::
  public x : Nat
  public y : Nat
deriving DecidableEq, Repr

public def Position.inBounds (map : Map) (position : Position) : Prop :=
  position.x < map.dimensions.width ∧ position.y < map.dimensions.height

public def Position.centeredOn (map : Map) : Position :=
  .mk (map.dimensions.width / 2) (map.dimensions.height / 2)

public theorem Position.centeredOn_in_bounds
    (map : Map) :
    (Position.centeredOn map).inBounds map := by
  constructor
  · exact Nat.div_lt_self map.dimensionsPositive.1 (by decide)
  · exact Nat.div_lt_self map.dimensionsPositive.2 (by decide)

public def Position.move (map : Map) (direction : Direction) (position : Position) : Position :=
  match direction with
  | .left =>
    if position.x = 0 then position else .mk (position.x - 1) position.y
  | .right =>
    if position.x + 1 < map.dimensions.width then .mk (position.x + 1) position.y else position
  | .up =>
    if position.y = 0 then position else .mk position.x (position.y - 1)
  | .down =>
    if position.y + 1 < map.dimensions.height then .mk position.x (position.y + 1) else position

public theorem Position.move_preserves_bounds
    (map : Map)
    (direction : Direction)
    (position : Position)
    (inBounds : position.inBounds map) :
    (position.move map direction).inBounds map := by
  cases direction with
  | left =>
    by_cases atLeft : position.x = 0
    · simpa [Position.inBounds, Position.move, atLeft] using inBounds
    · simp [Position.inBounds, Position.move, atLeft] at inBounds ⊢
      omega
  | right =>
    by_cases canMove : position.x + 1 < map.dimensions.width
    · simpa [Position.inBounds, Position.move, canMove] using
        And.intro canMove inBounds.2
    · simpa [Position.inBounds, Position.move, canMove] using inBounds
  | up =>
    by_cases atTop : position.y = 0
    · simpa [Position.inBounds, Position.move, atTop] using inBounds
    · simp [Position.inBounds, Position.move, atTop] at inBounds ⊢
      omega
  | down =>
    by_cases canMove : position.y + 1 < map.dimensions.height
    · simpa [Position.inBounds, Position.move, canMove] using
        And.intro inBounds.1 canMove
    · simpa [Position.inBounds, Position.move, canMove] using inBounds

end Gimlight
