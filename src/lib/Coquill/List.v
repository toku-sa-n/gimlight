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

Fixpoint repeat_pos {A : Type} (x : A) (n : positive) : list A :=
  match n with
  | xH => [x]
  | xO n' => repeat_pos x n' ++ repeat_pos x n'
  | xI n' => x::repeat_pos x n' ++ repeat_pos x n'
  end.

Theorem repeat_pos_length : forall {A : Type} (x : A) (n : positive), length (repeat_pos x n) = Npos n.
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

Theorem repeat_pos_spec : forall {A : Type} (n : positive) (x y : A), In y (repeat_pos x n) -> y = x.
Proof.
  intros.
  induction n.
  - simpl in H.
    destruct H.
    + auto.
    + apply IHn.
      apply in_app_or in H.
      destruct H; auto.
  - simpl in H.
    apply IHn.
    apply in_app_or in H.
    destruct H; auto.
  - simpl in H.
    destruct H; auto.
    destruct H.
Qed.

Theorem repeat_pos_non_empty {A : Type} (x : A) (n : positive) : repeat_pos x n <> [].
Proof.
  induction n.
  - simpl.
    intro H.
    inversion H.
  - simpl.
    intro H.
    apply app_eq_nil in H.
    destruct H.
    apply IHn.
    auto.
  - simpl.
    intro H.
    inversion H.
Qed.

Definition repeat {A : Type} (x : A) (n : N) : list A :=
  match n with
  | 0 => []
  | Npos p => repeat_pos x p
  end.

Theorem repeat_length : forall {A : Type} (x : A) (n : N), length (repeat x n) = n.
Proof.
  intros.
  destruct n; try reflexivity.
  apply repeat_pos_length.
Qed.

Theorem repeat_spec : forall {A : Type} (x : A) (n : N) (y : A), In y (repeat x n) -> y = x.
Proof.
  intros.
  destruct n.
  - simpl in H.
    destruct H.
  - apply repeat_pos_spec in H.
    auto.
Qed.
