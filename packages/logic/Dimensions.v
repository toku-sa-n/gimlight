From Stdlib Require Import BinPos.
Require Import DimensionsApi.

Module Dimensions : DimensionsApi.

Record t : Set := {
  width : positive;
  height : positive
}.

End Dimensions.
