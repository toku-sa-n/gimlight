module

namespace Gimlight

/-- A natural number in the inclusive interval `[min, max]`. -/
public abbrev NatRange (min max : Nat) :=
  { value : Nat // min ≤ value ∧ value ≤ max }

end Gimlight
