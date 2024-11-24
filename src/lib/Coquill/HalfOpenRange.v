Set Default Goal Selector "!".

From Coq Require Import NArith.

From Coquill Require Import PArith.

Open Scope N_scope.

(* `upper` can be `positive`, but using the same type as `lower` simplifies the proof. *)
Record t := make {
  lower : N;
  upper : N;
  lower_lt_upper : lower < upper
}.
