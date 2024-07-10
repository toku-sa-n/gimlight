Set Default Goal Selector "!".

From Coq Require Import Numbers.BinNums.

Record map : Type := mk_map {
  width : positive;
  height : positive;
}.
