module

namespace Gimlight

/-- A natural number in the half-open interval `[min, max)`. -/
public abbrev NatRange (min max : Nat) :=
  { value : Nat // min ≤ value ∧ value < max }

end Gimlight
