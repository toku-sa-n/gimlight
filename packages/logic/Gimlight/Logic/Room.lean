module

public import Gimlight.Logic.Position.Basic

namespace Gimlight

public structure Room where
  x : Nat
  y : Nat
  width : Nat
  height : Nat
deriving DecidableEq, Repr

public def Room.right (room : Room) : Nat := room.x + room.width - 1

public def Room.bottom (room : Room) : Nat := room.y + room.height - 1

public def Room.center (room : Room) : Position :=
  { x := room.x + room.width / 2, y := room.y + room.height / 2 }

public def Room.contains (room : Room) (x y : Nat) : Bool :=
  room.x ≤ x && x < room.x + room.width && room.y ≤ y && y < room.y + room.height

/-- Two rooms have at least one complete wall tile between their rectangles. -/
public def Room.separated (first second : Room) : Bool :=
  first.right + 1 < second.x || second.right + 1 < first.x ||
    first.bottom + 1 < second.y || second.bottom + 1 < first.y

public theorem Room.separated_symmetric (first second : Room) :
    first.separated second = second.separated first := by
  simp only [Room.separated, Bool.or_comm, Bool.or_left_comm]

public theorem Room.separated_has_wall_gap (first second : Room)
    (separated : first.separated second) :
    first.right + 1 < second.x ∨ second.right + 1 < first.x ∨
      first.bottom + 1 < second.y ∨ second.bottom + 1 < first.y := by
  simp only [Room.separated, Bool.or_eq_true] at separated
  rcases separated with ((left | right) | bottom) | top
  · exact Or.inl (of_decide_eq_true left)
  · exact Or.inr (Or.inl (of_decide_eq_true right))
  · exact Or.inr (Or.inr (Or.inl (of_decide_eq_true bottom)))
  · exact Or.inr (Or.inr (Or.inr (of_decide_eq_true top)))

end Gimlight
