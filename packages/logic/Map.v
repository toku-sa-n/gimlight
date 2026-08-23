From Stdlib Require Import BinPos.
Require Import Dimensions DimensionsApi MapApi.

Module MakeMap (Dimensions : DimensionsApi) : MapApi Dimensions.

Record t : Set := {
  dimensions : Dimensions.t
}.

Definition make (dimensions : Dimensions.t) : t :=
  {| dimensions := dimensions |}.

Definition default : t :=
  make {| Dimensions.width := Pos.of_nat 20;
          Dimensions.height := Pos.of_nat 10 |}.

End MakeMap.

Module Map : MapApi Dimensions :=
  MakeMap Dimensions.
