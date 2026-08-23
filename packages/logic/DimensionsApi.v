From Stdlib Require Import BinPos.

Module Type DimensionsApi.

Record t : Set := {
  width : positive;
  height : positive
}.

End DimensionsApi.
