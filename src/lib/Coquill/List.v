Set Default Goal Selector "!".

From Coq Require Import Lia.
From Coq Require Export List.
From Coq Require Import ProofIrrelevance.

From Coquill Require Import NArith.
From Coquill Require Import PArith.
From Coquill Require HalfOpenRange.

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

Section RepeatPos.
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
End RepeatPos.

Section Repeat.
  Context {A : Type}.

  Definition repeat (x : A) (n : N) : list A :=
    match n with
    | 0 => []
    | Npos p => repeat_pos x p
    end.

  Hint Unfold repeat : list.

  Theorem repeat_length : forall (x : A) (n : N), length (repeat x n) = n.
  Proof.
    intros.
    destruct n.
    - auto.
    - simpl.
      apply repeat_pos_length.
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

Section NthError.
  Context {A : Type}.

  Program Fixpoint nth_error (l : list A) (n : N) : option A :=
    match l with
    | [] => None
    | x :: l' =>
      match n with
      | 0 => Some x
      | Npos p => nth_error l' (N.pred n)
      end
    end.

  Theorem nth_error_some_in : forall (l : list A) (n : N) (x : A), nth_error l n = Some x -> In x l.
  Proof.
    induction l; intros.
    - discriminate.
    - destruct n.
      + simpl in H.
        injection H.
        intros.
        left.
        auto.
      + simpl in H.
        apply IHl in H.
        right.
        auto.
  Qed.

  Theorem nth_error_none_length : forall (l : list A) (n : N), nth_error l n = None -> n >= length l.
  Proof.
    induction l.
    - intros.
      simpl.
      lia.
    - destruct n.
      + intros.
        discriminate.
      + simpl.
        intros.
        apply IHl in H.
        lia.
  Qed.
End NthError.

Section Nth.
  Context {A : Type}.

  Program Definition nth (l : list A) (n : N) (n_spec : n < length l) : A :=
    match nth_error l n with
    | Some x => x
    | None => _
    end.
  Next Obligation.
  Proof.
    symmetry in Heq_anonymous.
    apply nth_error_none_length in Heq_anonymous.
    lia.
  Qed.

  Hint Unfold nth : list.

  Theorem nth_in : forall (l : list A) (n : N) H, In (nth l n H) l.
  Proof.
    intros.
    unfold nth.
    set (nth_obligation_1 _ _ _).
    clearbody a.
    simpl in a.
    destruct nth_error eqn:Heq.
    - apply nth_error_some_in in Heq.
      auto.
    - apply nth_error_none_length in Heq.
      lia.
  Qed.
End Nth.

Section Take.
  Context {A : Type}.

  Program Fixpoint take (l : list A) (n : positive) (n_spec : N.pos n <= length l) : list A :=
    match l with
    | [] => _
    | x :: l' =>
      match n with
      | xH => [x]
      | _ => x :: take l' (Pos.pred n) _
      end
    end.
  Next Obligation.
  Proof.
    simpl in n_spec.
    lia.
  Qed.
  Next Obligation.
  Proof.
    simpl in n_spec.
    lia.
  Qed.

  Hint Unfold take : list.

  Theorem take_length : forall (l : list A) n H, length (take l n H) = N.pos n.
  Proof.
    induction l; intros.
    - simpl in H.
      lia.
    - simpl.
      destruct n.
      + simpl.
        apply n_succ_m_pos_1.
        apply IHl.
      + simpl.
        apply n_succ_m_pos_0.
        apply IHl.
      + auto.
  Qed.
End Take.

Section Update.
  Context {A : Type}.

  Program Fixpoint update (l : list A) (n : N) (x : A) (n_spec : n < length l) : list A :=
    match l with
    | [] => _
    | y :: l' =>
      match n with
      | 0 => x :: l'
      | Npos p => y :: update l' (N.pred n) x _
      end
    end.
  Next Obligation.
  Proof.
    simpl in n_spec.
    lia.
  Qed.
  Next Obligation.
  Proof.
    simpl in n_spec.
    lia.
  Qed.

  Hint Unfold update : list.

  Theorem update_length : forall (l : list A) (n : N) (x : A) (n_spec : n < length l), length (update l n x n_spec) = length l.
  Proof.
    induction l; intros.
    - simpl in n_spec.
      lia.
    - simpl in n_spec.
      destruct n.
      + simpl.
        reflexivity.
      + simpl.
        f_equal.
        apply IHl.
  Qed.

  Hint Resolve update_length : list.

  Theorem update_in : forall (l : list A) (n : N) (x : A) (n_spec : n < length l), In x (update l n x n_spec).
  Proof.
    induction l; intros.
    - simpl in n_spec.
      lia.
    - simpl in n_spec.
      destruct n.
      + simpl.
        left.
        reflexivity.
      + simpl.
        right.
        apply IHl.
  Qed.

  Hint Resolve update_in : list.

  Theorem nth_error_update_eq : forall (l : list A) (n : N) (x y : A) H, nth_error (update l n x H) n = Some y -> x = y.
  Proof.
    induction l.
    - intros.
      simpl in H.
      lia.
    - intros.
      simpl in H0.
      destruct n.
      + simpl in H0.
        injection H0.
        intros.
        auto.
      + simpl in H0.
        apply IHl in H0.
        auto.
  Qed.

  Hint Resolve nth_error_update_eq : list.

  Theorem nth_update_eq : forall (l : list A) n x H H1, nth (update l n x H) n H1 = x.
  Proof.
    intros.
    unfold nth.
    set (nth_obligation_1 _ _ _).
    clearbody y.
    simpl in y.
    destruct nth_error eqn:Heq.
    - apply nth_error_update_eq in Heq.
      auto.
    - apply nth_error_none_length in Heq.
      lia.
  Qed.

  Hint Resolve nth_update_eq : list.

  Theorem nth_error_update_neq : forall (l : list A) (n m : N) (x : A) H, n <> m -> nth_error (update l n x H) m = nth_error l m.
  Proof.
    induction l.
    - intros.
      simpl in H.
      lia.
    - intros.
      simpl in H.
      destruct n.
      + simpl.
        destruct m.
        * lia.
        * auto.
      + simpl.
        destruct m.
        * auto.
        * apply IHl.
          lia.
  Qed.

  Hint Resolve nth_error_update_neq : list.

  Theorem nth_update_neq : forall (l : list A) n m x H H1 H2, n <> m -> nth (update l n x H) m H1 = nth l m H2.
  Proof.
    intros.
    unfold nth.
    set (nth_obligation_1 _ _ _).
    clearbody y.
    simpl in y.
    set (nth_obligation_1 _ _ _).
    clearbody y0.
    simpl in y0.
    destruct nth_error eqn:Heq.
    - destruct (nth_error l m) eqn:Heq'.
      + apply nth_error_update_neq with (l := l) (x := x) (H := H) in H0.
        rewrite H0 in Heq.
        rewrite Heq in Heq'.
        injection Heq'.
        intros.
        auto.
      + apply nth_error_none_length in Heq'.
        lia.
    - destruct (nth_error l m) eqn:Heq'.
      + apply nth_error_none_length in Heq.
        lia.
      + apply nth_error_none_length in Heq.
        lia.
  Qed.

  Hint Resolve nth_update_neq : list.
End Update.

Section UpdateFirstN.
  Context {A : Type}.

  Program Fixpoint update_first_n (l : list A) (n : positive) (x : A) (n_spec : N.pos n <= length l) : list A :=
    match l with
    | [] => _
    | y :: l' =>
      match n with
      | xH => x :: l'
      | _ => x :: update_first_n l' (Pos.pred n) x _
      end
    end.
  Next Obligation.
  Proof.
    simpl in n_spec.
    lia.
  Qed.
  Next Obligation.
  Proof.
    simpl in n_spec.
    lia.
  Qed.

  Hint Unfold update_first_n : list.

  Theorem update_first_n_length : forall (l : list A) n x H, length (update_first_n l n x H) = length l.
  Proof.
    induction l; intros.
    - simpl in H.
      lia.
    - simpl.
      destruct n.
      + simpl.
        f_equal.
        apply IHl.
      + simpl.
        f_equal.
        apply IHl.
      + auto.
  Qed.

  Hint Resolve update_first_n_length : list.

  Theorem update_first_n_in : forall (l : list A) n x H, In x (update_first_n l n x H).
  Proof.
    induction l; intros.
    - simpl in H.
      lia.
    - simpl.
      destruct n.
      + simpl.
        right.
        apply IHl.
      + simpl.
        right.
        apply IHl.
      + simpl.
        auto.
  Qed.

  Hint Resolve update_first_n_in : list.

  Theorem nth_error_update_first_n_eq : forall (l : list A) n idx x y H, idx < N.pos n -> nth_error (update_first_n l n x H) idx = Some y -> x = y.
  Proof.
    induction l.
    - intros.
      simpl in H.
      lia.
    - intros.
      simpl in H1.
      destruct n.
      + simpl in H1.
        destruct idx.
        * injection H1.
          intros.
          auto.
        * set (update_first_n_obligation_2 _ _ _ _ _ _ _ _ _) in H1.
          clearbody l0.
          simpl in H.
          simpl in l0.
          apply IHl with (n := xO n) (idx := (N.pred (N.pos p))) (H := l0).
          -- simpl.
             lia.
          -- auto.
      + set (update_first_n_obligation_2 _ _ _ _ _ _ _ _ _) in H1.
        clearbody l0.
        simpl in H1.
        destruct idx.
        * inversion H1.
          auto.
        * apply IHl with (n := (Pos.pred_double n)) (idx := (N.pred (N.pos p))) (H := l0).
          -- simpl.
             lia.
          -- auto.
      + simpl in H1.
        destruct idx.
        * injection H1.
          intros.
          auto.
        * lia.
  Qed.

  Hint Resolve nth_error_update_first_n_eq : list.

  Theorem nth_update_first_n_eq : forall (l : list A) n idx x H H1, idx < N.pos n -> nth (update_first_n l n x H) idx H1 = x.
  Proof.
    intros.
    unfold nth.
    set (nth_obligation_1 _ _ _).
    clearbody y.
    simpl in y.
    destruct nth_error eqn:Heq.
    - apply nth_error_update_first_n_eq in Heq; auto.
    - apply nth_error_none_length in Heq.
      lia.
  Qed.

  Hint Resolve nth_update_first_n_eq : list.

  Theorem nth_error_update_first_n_neq : forall (l : list A) n m x H, m >= N.pos n -> nth_error (update_first_n l n x H) m = nth_error l m.
  Proof.
    induction l.
    - intros.
      simpl in H.
      lia.
    - intros.
      simpl.
      destruct n eqn:Hn.
      + simpl.
        destruct m eqn:Hm.
        * lia.
        * simpl.
          apply IHl.
          lia.
      + simpl.
        destruct m eqn:Hm.
        * lia.
        * simpl.
          apply IHl.
          lia.
      + simpl.
        destruct m eqn:Hm.
        * lia.
        * auto.
  Qed.

  Theorem nth_update_first_n_neq : forall (l : list A) n m x H H1 H2, m >= N.pos n -> nth (update_first_n l n x H) m H1 = nth l m H2.
  Proof.
    intros.
    unfold nth.
    set (nth_obligation_1 _ _ _).
    clearbody y.
    simpl in y.
    set (nth_obligation_1 _ _ _).
    clearbody y0.
    simpl in y0.
    destruct nth_error eqn:Heq.
    - destruct (nth_error l m) eqn:Heq'.
      + apply nth_error_update_first_n_neq with (l := l) (x := x) (H := H) in H0.
        rewrite H0 in Heq.
        rewrite Heq in Heq'.
        injection Heq'.
        intros.
        auto.
      + apply nth_error_none_length in Heq'.
        lia.
    - destruct (nth_error l m) eqn:Heq'.
      + apply nth_error_none_length in Heq.
        lia.
      + apply nth_error_none_length in Heq.
        lia.
  Qed.
End UpdateFirstN.

Section UpdateRange.
  Context {A : Type}.

  Program Fixpoint update_range (l : list A) (r : HalfOpenRange.t) (x : A) (r_spec : HalfOpenRange.upper r <= length l) : list A :=
    match l with
    | [] => _
    | y :: l' =>
      match HalfOpenRange.contains r 0 with
      | true => update_first_n l (HalfOpenRange.length r) x _
      | false => y :: update_range l' (HalfOpenRange.shift_minus r 1 _) x _
      end
    end.
  Next Obligation.
