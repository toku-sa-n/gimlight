Set Default Goal Selector "!".

From Coq Require Import Numbers.BinNums.

Record position : Type := mk_position {
  x : positive;
  y : positive;
}.
