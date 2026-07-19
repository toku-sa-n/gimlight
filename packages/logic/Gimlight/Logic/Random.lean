module

public import Gimlight.Logic.NatInRange

namespace Gimlight

/--
Returns a random `Fin size`. Unlike `IO.rand`, which returns a `Nat` without a
proof that it lies in the requested range, this wrapper uses `Fin.ofNat`'s
normalization modulo `size` to construct the bound proof.
-/
public def Random.fin (size : Nat) (positive : 0 < size) : IO (Fin size) := do
  let value ← IO.rand 0 (size - 1)
  let _ : NeZero size := ⟨Nat.ne_of_gt positive⟩
  return Fin.ofNat size value

/-- Returns a uniformly random natural number in the half-open interval `[min, max)`. -/
public def Random.natRange (min max : Nat) (nonempty : min < max) : IO (NatInRange min max) := do
  let offset ← Random.fin (max - min) (by omega)
  return ⟨min + offset.val, by
    have offsetInRange := offset.isLt
    omega⟩

/-- Returns a uniformly random boolean. -/
public def Random.bool : IO Bool := do
  return (← IO.rand 0 1) == 0

end Gimlight
