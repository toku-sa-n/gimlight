From Stdlib Require Import ZArith.
Require Import Dimensions.

Module Map.

Record t : Set := {
  dimensions : Dimensions.t
}.

Definition make (dimensions : Dimensions.t) : t :=
  {| dimensions := dimensions |}.

Definition default : t :=
  make {| Dimensions.width := Pos.of_nat 20;
          Dimensions.height := Pos.of_nat 10 |}.

Lemma dimensions_positive (map : t) :
  0 < Dimensions.width_nat (dimensions map) /\
  0 < Dimensions.height_nat (dimensions map).
Proof.
  split.
  - apply Dimensions.width_positive.
  - apply Dimensions.height_positive.
Qed.

End Map.
