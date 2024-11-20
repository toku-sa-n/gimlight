From Coq Require Import NArith.
From Coquill Require Import PArith.

From Coq Require Export List.
Export ListNotations.

Open Scope N_scope.

Fixpoint length {A : Type} (l : list A) : N :=
  match l with
  | [] => 0
  | _ :: l' => N.succ (length l')
  end.

Lemma app_length : forall {A : Type} (l1 l2 : list A), length (l1 ++ l2) = length l1 + length l2.
Proof.
  induction l1.
  - reflexivity.
  - simpl.
    intros.
    rewrite IHl1.
    rewrite N.add_succ_l.
    reflexivity.
Qed.

Fixpoint repeat {A : Type} (x : A) (n : positive) : list A :=
  match n with
  | xH => [x]
  | xO n' => repeat x n' ++ repeat x n'
  | xI n' => x::repeat x n' ++ repeat x n'
  end.

Theorem repeat_length : forall {A : Type} (x : A) (n : positive), length (repeat x n) = Npos n.
Proof.
  induction n; try reflexivity.
  - simpl.
    rewrite app_length.
    rewrite IHn.
    unfold N.succ.
    simpl.
    f_equal.
    apply double_succ.
  - simpl.
    rewrite app_length.
    rewrite IHn.
    simpl.
    f_equal.
    apply double_0.
Qed.
