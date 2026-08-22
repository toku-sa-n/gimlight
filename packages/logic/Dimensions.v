From Stdlib Require Import BinPos ZArith.

Module Dimensions.

Record t : Set := {
  width : positive;
  height : positive
}.

Definition width_nat (dimensions : t) : nat :=
  Pos.to_nat (width dimensions).

Definition height_nat (dimensions : t) : nat :=
  Pos.to_nat (height dimensions).

End Dimensions.
