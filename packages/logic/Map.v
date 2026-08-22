From Stdlib Require Import ZArith.
Require Import Dimensions.

Module Type MapApi.

Record t : Set := {
  dimensions : Dimensions.t
}.

Parameter make : Dimensions.t -> t.

Parameter default : t.

End MapApi.

Module Map : MapApi.

Record t : Set := {
  dimensions : Dimensions.t
}.

Definition make (dimensions : Dimensions.t) : t :=
  {| dimensions := dimensions |}.

Definition default : t :=
  make {| Dimensions.width := Pos.of_nat 20;
          Dimensions.height := Pos.of_nat 10 |}.

End Map.
