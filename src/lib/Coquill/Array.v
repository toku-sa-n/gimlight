Set Default Goal Selector "!". 

From Coq Require Import NArith.

From Coquill Require Import List.
From Coquill Require HalfOpenRange.

Open Scope N_scope.

Create HintDb array.

Record t (A : Type) (n : N) : Type := make {
  inner_list : list A;
  length_spec : length inner_list = n;
}.

Arguments make {A n} inner_list length_spec.
Arguments inner_list {A n} _.
Arguments length_spec {A n} _.

Hint Constructors t : array.

Definition empty (A : Type) : t A 0 := make [] eq_refl.

Program Definition repeat {A : Type} (x : A) (n : N) : t A n :=
  make (List.repeat x n) _.
Next Obligation.
Proof.
  apply repeat_length.
Qed.

Section UpdateRange.
  Context {A : Type}.
  Context {n : N}.

  Program Definition update_range (arr : t A n) (r : HalfOpenRange.t) (x : A) (upper_spec : HalfOpenRange.upper r <= n) : t A n :=
    make (List.update_range (inner_list arr) r x _) _.
  Next Obligation.
  Proof.
    rewrite length_spec.
    auto.
  Qed.
  Next Obligation.
  Proof.
    rewrite length_update_range.
    apply length_spec.
  Qed. 
End UpdateRange.

Section MapNth.
  Context {A : Type}.
  Context {n : N}.

  Program Definition map_nth (arr : t A n) (i : N) (f : A -> A) (i_spec : i < n) : t A n :=
    make (List.map_nth (inner_list arr) i f _) _.
  Next Obligation.
  Proof.
    rewrite length_spec.
    auto.
  Qed.
  Next Obligation.
  Proof.
    rewrite length_map_nth.
    apply length_spec.
  Qed.
End MapNth.

Section Update.
  Context {A : Type}.
  Context {n : N}.

  Program Definition update (arr : t A n) (i : N) (x : A) (i_spec : i < n) : t A n :=
    make (List.update (inner_list arr) i x _) _.
  Next Obligation.
  Proof.
    rewrite length_spec.
    auto.
  Qed.
  Next Obligation.
  Proof.
    rewrite length_update.
    apply length_spec.
  Qed.
End Update.
