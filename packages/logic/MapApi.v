Require Import Dimensions.

Module Type MapApi.

Record t : Set := {
  dimensions : Dimensions.t
}.

Parameter make : Dimensions.t -> t.

Parameter default : t.

End MapApi.
