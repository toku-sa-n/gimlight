module

namespace Gimlight

public def Random.fin (size : Nat) (positive : 0 < size) : IO (Fin size) := do
  let value ← IO.rand 0 (size - 1)
  let _ : NeZero size := ⟨Nat.ne_of_gt positive⟩
  -- `IO.rand` returns a `Nat` without proof that it lies in the requested range.
  -- This wrapper returns `Fin size`, using `Fin.ofNat`'s `% size` normalization
  -- to construct the bound proof.
  return Fin.ofNat size value

end Gimlight
