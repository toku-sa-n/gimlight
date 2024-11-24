Set Default Goal Selector "!".

From Coq Require Import NArith.
From Coquill Require Import PArith.

From Coq Require Export List.
Export ListNotations.

Open Scope N_scope.

Create HintDb list.

Section Length.
  Context {A : Type}.

  Fixpoint length (l : list A) : N :=
    match l with
    | [] => 0
    | _ :: l' => N.succ (length l')
    end.

  Hint Unfold length : list.

  Lemma app_length : forall (l1 l2 : list A), length (l1 ++ l2) = length l1 + length l2.
  Proof.
    induction l1.
    - reflexivity.
    - simpl.
      intros.
      rewrite IHl1.
      rewrite N.add_succ_l.
      auto.
  Qed.

  Hint Resolve app_length : list.
End Length.

Section Repeat.
  Context {A : Type}.

  Fixpoint repeat_pos (x : A) (n : positive) : list A :=
    match n with
    | xH => [x]
    | xO n' => repeat_pos x n' ++ repeat_pos x n'
    | xI n' => x::repeat_pos x n' ++ repeat_pos x n'
    end.

  Hint Unfold repeat_pos : list.

  Theorem repeat_pos_length : forall (x : A) (n : positive), length (repeat_pos x n) = Npos n.
  Proof.
    induction n; try reflexivity; simpl; rewrite app_length; rewrite IHn; unfold N.succ; simpl; f_equal; auto with positive.
  Qed.

  Hint Resolve repeat_pos_length : list.

  Theorem repeat_pos_spec : forall (n : positive) (x y : A), In y (repeat_pos x n) -> y = x.
  Proof.
    intros.
    induction n; simpl in H; try destruct H; auto; try (apply IHn; apply in_app_or in H); destruct H; auto.
  Qed.

  Hint Resolve repeat_pos_spec : list.

  Theorem repeat_pos_non_empty (x : A) (n : positive) : repeat_pos x n <> [].
  Proof.
    induction n; simpl; intros H; try inversion H.
    apply app_eq_nil in H.
    easy.
  Qed.

  Hint Resolve repeat_pos_non_empty : list.

  Definition repeat (x : A) (n : N) : list A :=
    match n with
    | 0 => []
    | Npos p => repeat_pos x p
    end.

  Hint Unfold repeat : list.

  Theorem repeat_length : forall (x : A) (n : N), length (repeat x n) = n.
  Proof.
    intros.
    destruct n; eauto with list.
  Qed.

  Hint Resolve repeat_length : list.

  Theorem repeat_spec : forall (x : A) (n : N) (y : A), In y (repeat x n) -> y = x.
  Proof.
    intros.
    destruct n.
    - easy.
    - apply repeat_pos_spec in H.
      easy.
  Qed.
End Repeat.
