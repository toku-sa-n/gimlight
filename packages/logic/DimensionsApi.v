From Stdlib Require Import BinPos ZArith.

Module Type DimensionsApi.

Record t : Set := {
  width : positive;
  height : positive
}.

End DimensionsApi.
