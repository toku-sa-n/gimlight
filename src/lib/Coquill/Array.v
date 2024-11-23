Set Default Goal Selector "!". 

From Coq Require Import NArith.
From Coquill Require Import List.

Open Scope N_scope.

Create HintDb array.

Record t (A : Type) (n : N) : Type := make {
  inner_list : list A;
  length_spec : length inner_list = n;
}.

Hint Constructors t : array.

Definition empty (A : Type) : t A 0 := make A 0 [] eq_refl.

Program Definition repeat {A : Type} (x : A) (n : N) : t A n :=
  make A n (List.repeat x n) _.
Next Obligation.
Proof.
  apply repeat_length.
Qed.
