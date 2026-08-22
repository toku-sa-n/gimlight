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

Lemma width_positive (dimensions : t) :
  0 < width_nat dimensions.
Proof.
  unfold width_nat.
  apply Pos2Nat.is_pos.
Qed.

Lemma height_positive (dimensions : t) :
  0 < height_nat dimensions.
Proof.
  unfold height_nat.
  apply Pos2Nat.is_pos.
Qed.

End Dimensions.
