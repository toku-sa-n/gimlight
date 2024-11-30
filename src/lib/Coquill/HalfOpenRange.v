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

Definition contains (r : t) (n : N) : Prop :=
  lower r <= n < upper r.

Definition containsb (r : t) (n : N) : bool :=
  (lower r <=? n) && (n <? upper r).

Theorem containsb_contains (r : t) (n : N) : containsb r n = true <-> contains r n.
Proof.
  unfold contains, containsb.
  split.
  - intros.
    apply andb_prop in H.
    destruct H.
    rewrite N.leb_le in H.
    rewrite N.ltb_lt in H0.
    apply conj; auto.
  - intros.
    apply andb_true_intro.
    destruct H.
    rewrite <- N.leb_le in H.
    rewrite <- N.ltb_lt in H0.
    split; auto.
Qed.

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

Theorem length_le_upper : forall (r : t), N.pos (length r) <= upper r.
Proof.
  intros.
  unfold length.
  set (length_obligation_1 _).
  clearbody p.
  simpl in p.
  destruct (upper r - lower r) eqn:E.
  - assert (lower r < upper r) by apply lower_lt_upper.
    lia.
  - lia.
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

Theorem upper_is_positive (r : t) : upper r > 0.
Proof.
  assert (lower r < upper r) by apply lower_lt_upper.
  lia.
Qed.

Theorem lower_0_length_eq_upper (r : t) : lower r = 0 -> N.pos (length r) = upper r.
Proof.
  intros.
  unfold length.
  set (length_obligation_1 _).
  clearbody p.
  simpl in p.
  destruct (upper r - lower r) eqn:E.
  - assert (lower r < upper r) by apply lower_lt_upper.
    lia.
  - lia.
Qed.
