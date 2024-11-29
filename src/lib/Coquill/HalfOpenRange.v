Set Default Goal Selector "!".

From Coq Require Import NArith.
From Coq Require Import Lia.

From Coquill Require Import PArith.

Open Scope N_scope.

(* `upper` can be `positive`, but using the same type as `lower` simplifies the proof. *)
Record t := make {
  lower : N;
  upper : N;
  lower_lt_upper : lower < upper
}.

Definition contains (r : t) (n : N) : bool :=
  (lower r <=? n) && (n <? upper r).

Program Definition length (r : t) : positive := 
  let l : N := upper r - lower r in
  match l with
  | N0 => _
  | Npos p => p
  end.
Next Obligation.
  assert (lower r < upper r) by apply lower_lt_upper.
  lia.
Qed.

Program Definition shift_minus (r : t) (n : N) (H : n <= lower r) : t :=
  make (lower r - n) (upper r - n) _.
Next Obligation.
  apply N.add_lt_mono_r with (p := n).
  replace (lower r - n + n) with (lower r) by lia.
  replace (upper r - n + n) with (upper r).
  - apply lower_lt_upper.
  - assert (n < upper r).
    {
      assert (lower r < upper r) by apply lower_lt_upper.
      lia.
    }
    lia.
  Qed.
