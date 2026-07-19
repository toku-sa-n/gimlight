module

namespace Gimlight

/-- A natural number in the half-open interval `[min, max)`. -/
public abbrev NatInRange (min max : Nat) :=
  { value : Nat // min ≤ value ∧ value < max }

end Gimlight
