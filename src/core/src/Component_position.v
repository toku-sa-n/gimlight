Set Default Goal Selector "!".

From Coq Require Import ZArith.ZArith.

Record position : Type := mk_position {
  x : Z;
  y : Z;
}.
