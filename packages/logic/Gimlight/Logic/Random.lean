module

namespace Gimlight

public def Random.fin (size : Nat) (positive : 0 < size) : IO (Fin size) := do
  let value ← IO.rand 0 (size - 1)
  let _ : NeZero size := ⟨Nat.ne_of_gt positive⟩
  -- `IO.rand` does not encode its range guarantee in its result type, so use
  -- `Fin.ofNat` to normalize the value with `% size` and obtain the bound proof.
  return Fin.ofNat size value

end Gimlight
