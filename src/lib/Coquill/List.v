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

  Theorem nth_error_some_length : forall (l : list A) (n : N) (x : A), nth_error l n = Some x -> n < length l.
  Proof.
    induction l; intros.
    - discriminate.
    - destruct n.
      + simpl.
        lia.
      + simpl.
        simpl in H.
        apply IHl in H.
        lia.
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

  Theorem nth_error_nth : forall (l : list A) (n : N) H, nth_error l n = Some (nth l n H).
  Proof.
    intros.
    unfold nth.
    set (nth_obligation_1 _ _ _).
    clearbody a.
    simpl in a.
    destruct nth_error eqn:Heq.
    - apply nth_error_some_length in Heq.
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

  Theorem length_update : forall (l : list A) (n : N) (x : A) (n_spec : n < length l), length (update l n x n_spec) = length l.
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

  Hint Resolve length_update : list.

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

  Theorem length_update_first_n : forall (l : list A) n x H, length (update_first_n l n x H) = length l.
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

  Hint Resolve update_first_n : list.

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

  Theorem nth_error_update_first_n_eq : forall (l : list A) n idx x H, idx < N.pos n -> nth_error (update_first_n l n x H) idx = Some x.
  Proof.
    induction l.
    - intros.
      simpl in H.
      lia.
    - intros.
      simpl.
      destruct n eqn:En.
      + simpl.
        destruct idx.
        * auto.
        * simpl.
          apply IHl.
          lia.
      + simpl.
        destruct idx.
        * auto.
        * simpl.
          apply IHl.
          lia.
      + simpl.
        destruct idx eqn:Eidx.
        * auto.
        * simpl.
          lia.
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
    - assert (nth_error (update_first_n l n x H) idx = Some x).
      {
        apply nth_error_update_first_n_eq.
        auto.
      }
      rewrite Heq in H2.
      injection H2.
      intros.
      auto.
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
      match HalfOpenRange.lower r =? 0 with
      | true => update_first_n l (HalfOpenRange.length r) x _
      | false => y :: update_range l' (HalfOpenRange.shift_minus r 1 _) x _
      end
    end.
  Next Obligation.
  Proof.
    simpl in r_spec.
    assert (HalfOpenRange.upper r > 0) by apply HalfOpenRange.upper_is_positive.
    lia.
  Qed.
  Next Obligation.
  Proof.
    simpl in r_spec.
    simpl.
    assert (N.pos (HalfOpenRange.length r) <= HalfOpenRange.upper r) by apply HalfOpenRange.length_le_upper.
    lia.
  Qed.
  Next Obligation.
  Proof.
    unfold HalfOpenRange.contains in Heq_anonymous.
    symmetry in Heq_anonymous.
    apply N.eqb_neq in Heq_anonymous.
    lia.
  Qed.
  Next Obligation.
  Proof.
    simpl in r_spec.
    lia.
  Qed.

  Theorem length_update_range : forall (l : list A) r x H, length (update_range l r x H) = length l.
  Proof.
    induction l.
    - intros.
      assert (HalfOpenRange.upper r > 0) by apply HalfOpenRange.upper_is_positive.
      simpl in H.
      lia.
    - intros.
      simpl.
      set (update_first_n_obligation_2 _ _ _ _ _ _).
      clearbody l0.
      set (update_range_obligation_3 _ _ _ _ _ _).
      clearbody l1.
      simpl in l1.
      set (update_range_obligation_4 _ _ _ _ _ _).
      clearbody l2.
      simpl in l2.
      destruct (HalfOpenRange.lower r =? 0) eqn:E.
      + destruct (HalfOpenRange.length r) eqn:E'.
        * simpl.
          f_equal.
          apply length_update_first_n.
        * simpl.
          f_equal.
          apply length_update_first_n.
        * auto.
      + simpl.
        f_equal.
        apply IHl.
  Qed.

  Theorem lower_0_update_range_update_first_n : forall (l : list A) r x H H1, (HalfOpenRange.lower r = 0) -> update_range l r x H = update_first_n l (HalfOpenRange.length r) x H1.
  Proof.
    induction l.
    - intros.
      simpl in H1.
      lia.
    - intros.
      simpl.
      set (update_first_n_obligation_2 _ _ _ _ _ _).
      clearbody l0.
      simpl in l0.
      set (update_range_obligation_3 _ _ _ _ _ _).
      clearbody l1.
      simpl in l1.
      set (update_range_obligation_4 _ _ _ _ _ _).
      clearbody l2.
      simpl in l2.
      set (update_first_n_obligation_2 _ _ _ _ _ _).
      clearbody l3.
      simpl in l3.
      destruct (HalfOpenRange.lower r =? 0) eqn:E.
      + destruct (HalfOpenRange.length r) eqn:E'.
        * simpl.
          f_equal.
          assert (l0 = l3).
          {
            apply proof_irrelevance.
          }
          rewrite H2.
          auto.
        * simpl.
          f_equal.
          assert (l0 = l3).
          {
            apply proof_irrelevance.
          }
          rewrite H2.
          auto.
        * auto.
      + destruct (HalfOpenRange.length r) eqn:E'.
        * simpl.
          rewrite N.eqb_neq in E.
          lia.
        * simpl.
          rewrite N.eqb_neq in E.
          lia.
        * simpl.
          rewrite N.eqb_neq in E.
          lia.
  Qed.

  Theorem nth_error_update_range : forall (l : list A) r x idx H, (HalfOpenRange.contains r idx) -> nth_error (update_range l r x H) idx = Some x.
  Proof.
    induction l.
    - intros.
      simpl in H.
      assert (HalfOpenRange.upper r > 0) by apply HalfOpenRange.upper_is_positive.
      lia.
    - intros.
      unfold update_range.
      set (update_range_obligation_2 _ _ _ _ _ _).
      clearbody l0.
      simpl in l0.
      set (update_range_obligation_3 _ _ _ _ _ _).
      clearbody l1.
      simpl in l1.
      set (update_range_obligation_4 _ _ _ _ _ _).
      clearbody l2.
      simpl in l2.
      destruct (HalfOpenRange.lower r =? 0) eqn:E.
      + apply nth_error_update_first_n_eq.
        simpl in *.
        unfold HalfOpenRange.contains in H0.
        destruct H0.
        rewrite HalfOpenRange.lower_0_length_eq_upper; auto.
        rewrite N.eqb_eq in E.
        auto.
      + rewrite N.eqb_neq in E.
        unfold HalfOpenRange.contains in H0.
        simpl.
        destruct idx eqn:E'.
        * lia.
        * apply IHl. 
          simpl.
          unfold HalfOpenRange.contains.
          split.
          -- simpl.
             lia.
          -- simpl.
             lia.
  Qed.

  Theorem nth_update_range : forall (l : list A) r x idx H H1, (HalfOpenRange.contains r idx) -> nth (update_range l r x H) idx H1 = x.
  Proof.
    intros.
    unfold nth.
    set (nth_obligation_1 _ _ _).
    clearbody y.
    simpl in y.
    destruct nth_error eqn:Heq.
    - rewrite nth_error_update_range in Heq.
      + injection Heq.
        intros.
        auto.
      + auto.
    - apply nth_error_none_length in Heq.
      lia.
  Qed.

  Theorem nth_error_update_range_neq : forall (l : list A) r x idx H, ~ (HalfOpenRange.contains r idx) -> nth_error (update_range l r x H) idx = nth_error l idx.
  Proof.
    induction l.
    - intros.
      simpl in H.
      assert (HalfOpenRange.upper r > 0) by apply HalfOpenRange.upper_is_positive.
      lia.
    - intros.
      unfold update_range.
      set (update_range_obligation_2 _ _ _ _ _ _).
      clearbody l0.
      simpl in l0.
      set (update_range_obligation_3 _ _ _ _ _ _).
      clearbody l1.
      simpl in l1.
      set (update_range_obligation_4 _ _ _ _ _ _).
      clearbody l2.
      simpl in l2.
      destruct (HalfOpenRange.lower r =? 0) eqn:E.
      + apply nth_error_update_first_n_neq.
        assert (N.pos (HalfOpenRange.length r) = HalfOpenRange.upper r).
        {
          apply HalfOpenRange.lower_0_length_eq_upper.
          rewrite N.eqb_eq in E.
          auto.
        }
        rewrite H1.
        apply (HalfOpenRange.lower_0_not_contained_gt r idx).
        * rewrite N.eqb_eq in E.
          auto.
        * auto.
      + simpl.
        destruct idx eqn:E'.
        * auto.
        * apply IHl.
          simpl.
          unfold HalfOpenRange.contains.
          intro.
          unfold HalfOpenRange.shift_minus in H1.
          simpl in H1.
          unfold HalfOpenRange.contains in H0.
          lia.
  Qed.

  Theorem nth_update_range_neq : forall (l : list A) r x idx H H1 H2, ~ (HalfOpenRange.contains r idx) -> nth (update_range l r x H) idx H1 = nth l idx H2.
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
    - destruct (nth_error l idx) eqn:Heq'.
      + apply nth_error_update_range_neq with (l := l) (r := r) (x := x) (H := H) in H0.
        rewrite H0 in Heq.
        rewrite Heq in Heq'.
        injection Heq'.
        intros.
        auto.
      + apply nth_error_none_length in Heq'.
        lia.
    - destruct (nth_error l idx) eqn:Heq'.
      + apply nth_error_none_length in Heq.
        lia.
      + apply nth_error_none_length in Heq.
        lia.
  Qed.
End UpdateRange.

Section MapNth.
  Context {A : Type}.

  Program Fixpoint map_nth (l : list A) (n : N) (f : A -> A) (n_spec : n < length l) : list A :=
    match l with
    | [] => _
    | x :: l' =>
      match n with
      | 0 => f x :: l'
      | Npos p => x :: map_nth l' (N.pred n) f _
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

  Theorem length_map_nth : forall (l : list A) n f H, length (map_nth l n f H) = length l.
  Proof.
    induction l.
    - intros.
      simpl in H.
      lia.
    - intros.
      simpl.
      destruct n.
      + simpl.
        reflexivity.
      + simpl.
        f_equal.
        apply IHl.
  Qed.

  Theorem nth_error_map_nth : forall (l : list A) n f H, nth_error (map_nth l n f H) n = option_map f (nth_error l n).
  Proof.
    induction l.
    - intros.
      simpl in H.
      lia.
    - intros.
      simpl.
      destruct n.
      + simpl.
        reflexivity.
      + simpl.
        apply IHl.
  Qed.

  Theorem nth_map_nth : forall (l : list A) n f H H1, nth (map_nth l n f H) n H1 = f (nth l n H).
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
    - destruct (nth_error l n) eqn:Heq'.
      + rewrite nth_error_map_nth in Heq.
        rewrite Heq' in Heq.
        injection Heq.
        intros.
        auto.
      + rewrite nth_error_map_nth in Heq.
        rewrite Heq' in Heq.
        discriminate.
    - destruct (nth_error l n) eqn:Heq'.
      + rewrite nth_error_map_nth in Heq.
        rewrite Heq' in Heq.
        discriminate.
      + apply nth_error_none_length in H1; try lia.
        auto.
  Qed.

  Theorem map_nth_update_nth_error : forall (l : list A) n x f H, nth_error l n = Some x -> map_nth l n f H = update l n (f x) H.
  Proof.
    induction l.
    - intros.
      discriminate.
    - intros.
      simpl in H.
      destruct n.
      + simpl.
        simpl in H0.
        injection H0.
        intros.
        f_equal.
        f_equal.
        auto.
      + simpl.
        f_equal.
        set (map_nth_obligation_2 _ _ _ _ _ _ _ _).
        clearbody l0.
        simpl in l0.
        set (update_obligation_2 _ _ _ _ _ _ _ _).
        clearbody l1.
        simpl in l1.
        simpl in H0.
        eapply IHl in H0.
        assert (l0 = l1).
        {
          apply proof_irrelevance.
        }
        rewrite H1.
        apply H0.
  Qed.

  Theorem map_nth_update_nth : forall (l : list A) n f H H1, map_nth l n f H = update l n (f (nth l n H1)) H.
  Proof.
    intros.
    destruct (nth_error l n) eqn:Heq.
    - assert (map_nth l n f H = update l n (f a) H).
      {
        apply map_nth_update_nth_error.
        auto.
      }
      rewrite H0.
      f_equal.
      f_equal.
      erewrite nth_error_nth in Heq.
      injection Heq.
      intros.
      symmetry.
      apply H2.
    - apply nth_error_none_length in Heq.
      lia.
  Qed.

  Theorem map_nth_id : forall (l : list A) n H, map_nth l n (fun x => x) H = l.
  Proof.
    induction l.
    - intros.
      simpl in H.
      lia.
    - intros.
      simpl.
      destruct n.
      + auto.
      + f_equal.
        apply IHl.
  Qed.
End MapNth.
