From Stdlib Require Import BinPos ZArith.

Module Type DimensionsApi.

Record t : Set := {
  width : positive;
  height : positive
}.

Parameter width_nat : t -> nat.

Parameter height_nat : t -> nat.

End DimensionsApi.

Module Dimensions : DimensionsApi.

Record t : Set := {
  width : positive;
  height : positive
}.

Definition width_nat (dimensions : t) : nat :=
  Pos.to_nat (width dimensions).

Definition height_nat (dimensions : t) : nat :=
  Pos.to_nat (height dimensions).

End Dimensions.
