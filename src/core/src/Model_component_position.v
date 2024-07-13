Set Default Goal Selector "!".

From Coq Require Import Numbers.BinNums.

(* (0, 0) is the top left corner of the map. *)
Record position : Type := mk_position {
  x : N;
  y : N;
}.
