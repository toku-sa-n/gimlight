Set Default Goal Selector "!".

From Coq Require Import Lia.
From Coq Require Export List.
From Coq Require Import ProofIrrelevance.

From Coquill Require Import NArith.
From Coquill Require Import PArith.

From Coquill Require HalfOpenRange.

Export ListNotations.

Open Scope N_scope.

Section Length.
  Context {A : Type}.

  Fixpoint length (l : list A) : N :=
match l with
    | [] => 0
    | _ :: l' => N.succ (length l')
    end.

  Theorem app_length : forall (l1 l2 : list A), length (l1 ++ l2) = length l1 + length l2.
    Proof.
      induction l1.
      - reflexivity.
      - simpl.
        intros.
        rewrite IHl1.
        rewrite N.add_succ_l.
        auto.
  Qed.

  Theorem length_zero_iff_nil : forall (l : list A), length l = 0 <-> l = [].
  Proof.
    split.
    - destruct l.
      + auto.
      + simpl.
        lia.
    - intros.
      rewrite H.
      auto.
  Qed.
End Length.

Section RepeatPos.
  Context {A : Type}.

  Fixpoint repeat_pos (x : A) (n : positive) : list A :=
    match n with
    | xH => [x]
    | xO n' => repeat_pos x n' ++ repeat_pos x n'
    | xI n' => x::repeat_pos x n' ++ repeat_pos x n'
    end.

  Theorem length_repeat_pos : forall (x : A) (n : positive), length (repeat_pos x n) = Npos n.
  Proof.
    induction n; auto; simpl; rewrite app_length; rewrite IHn; lia.
  Qed.

  Theorem repeat_pos_in : forall (n : positive) (x y : A), In y (repeat_pos x n) -> y = x.
  Proof.
    induction n; intros; simpl in H.
    - destruct H; auto.
      apply IHn.
      apply in_app_or in H.
      destruct H; auto.
    - apply in_app_or in H.
      destruct H; auto.
    - destruct H; auto.
      destruct H.
  Qed.

  Theorem repeat_pos_non_empty (x : A) (n : positive) : repeat_pos x n <> [].
  Proof.
    induction n; simpl; try discriminate.
    intros H.
    apply IHn.
    apply app_eq_nil in H.
    destruct H.
    auto.
  Qed.
End RepeatPos.

Section Repeat.
  Context {A : Type}.

  Definition repeat (x : A) (n : N) : list A :=
    match n with
    | 0 => []
    | Npos p => repeat_pos x p
    end.

  Theorem length_repeat : forall (x : A) (n : N), length (repeat x n) = n.
  Proof.
    intros.
    destruct n; auto.
    simpl.
    apply length_repeat_pos.
  Qed.

  Theorem repeat_in : forall (x : A) (n : N) (y : A), In y (repeat x n) -> y = x.
  Proof.
    intros.
    destruct n.
    - easy.
    - apply repeat_pos_in in H.
      easy.
  Qed.

  Theorem repeat_empty_iff_zero : forall (x : A) (n : N), repeat x n = [] <-> n = 0.
  Proof.
    intros.
    split.
    - intros.
      destruct n.
      + easy.
      + simpl in H.
        apply repeat_pos_non_empty in H.
        easy.
    - intros.
      rewrite H.
      easy.
  Qed.
End Repeat.

Section Nth.
  Context {A : Type}.

  Program Fixpoint nth (l : list A) (n : N) (n_spec : n < length l) : A :=
    match l, n with
    | h :: _, 0 => h
    | _ :: t, Npos p => nth t (N.pred n) _
    | [], _ => _
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


  Theorem nth_in : forall (l : list A) (n : N) H, In (nth l n H) l.
  Proof.
    induction l.
    - intros.
      simpl in H.
      lia.
    - simpl.
      destruct n.
      + left.
        easy.
      + right.
        apply IHl.
  Qed.
End Nth.

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

  Theorem length_update : forall (l : list A) (n : N) (x : A) (n_spec : n < length l), length (update l n x n_spec) = length l.
  Proof.
    induction l; intros; simpl in *.
    - lia.
    - destruct n; simpl in *.
      + easy.
      + simpl.
        f_equal.
        easy.
  Qed.

  Theorem update_in : forall (l : list A) (n : N) (x : A) (n_spec : n < length l), In x (update l n x n_spec).
  Proof.
    induction l; intros; simpl in *.
    - lia.
    - destruct n; simpl.
      + left.
        easy.
      + right.
        easy.
  Qed.

  Theorem nth_update_eq : forall (l : list A) n x H H1, nth (update l n x H) n H1 = x.
  Proof.
    induction l; intros; simpl in *.
    - lia.
    - destruct n.
      + easy.
      + apply IHl.
  Qed.

  Theorem nth_update_neq : forall (l : list A) n m x H H1 H2, n <> m -> nth (update l n x H) m H1 = nth l m H2.
  Proof.
    induction l; intros; simpl in *.
    - lia.
    - destruct n, m.
      + lia.
      + simpl.
        set (nth_obligation_1 _ _ _ _ _ _ _ _).
        set (nth_obligation_1 _ _ _ _ _ _ _ _).
        clearbody l0.
        clearbody l1.
        assert (l0 = l1).
        {
          apply proof_irrelevance.
        }
        now rewrite H3.
      + easy.
      + simpl.
        apply IHl.
        lia.
  Qed.
End Update.

Section UpdateRange.
  Context {A : Type}.

  Program Fixpoint update_range (l : list A) (lower upper : N) (x : A) (H : 0 <= lower /\ lower < upper /\ upper <= length l) : list A :=
    match l, lower, upper with
    | [], _, _ => _
    | h :: t, 0, 0 => _
    | h :: t, 0, Npos xH => x :: t
    | h :: t, 0, Npos (xO _) => x :: update_range t 0 (N.pred upper) x _
    | h :: t, 0, Npos (xI _) => x :: update_range t 0 (N.pred upper) x _
    | h :: t, Npos _, _ => h :: update_range t (N.pred lower) (N.pred upper) x _
    end.
  Next Obligation.
  Proof.
    simpl in H1.
    lia.
  Qed.
  Next Obligation.
  Proof.
    split; simpl in *; lia.
  Qed.
  Next Obligation.
  Proof.
    split; simpl in *; lia.
  Qed.
  Next Obligation.
  Proof.
    split; simpl in *; lia.
  Qed.

  Theorem length_update_range : forall (l : list A) lower upper x H, length (update_range l lower upper x H) = length l.
  Proof.
    induction l; intros; simpl in *.
    - lia.
    - destruct lower, upper; try lia; destruct p eqn:Ep; simpl; f_equal; easy.
  Qed.

  Theorem nth_update_range_eq : forall (l : list A) lower upper x idx H H1, lower <= idx < upper -> nth (update_range l lower upper x H) idx H1 = x.
  Proof.
    induction l; intros; simpl in *.
    - lia.
    - destruct lower, upper; simpl.
      + lia.
      + destruct p, idx; simpl; try apply IHl; try lia; easy.
      + destruct idx.
        * lia.
        * simpl.
          apply IHl.
          lia.
      + destruct idx.
        * easy.
        * simpl.
          apply IHl.
          lia.
  Qed.

  Theorem nth_update_range_neq : forall (l : list A) lower upper x idx H H1 H2, ~ (lower <= idx < upper) -> nth (update_range l lower upper x H) idx H1 = nth l idx H2.
  Proof.
    induction l; intros; simpl in *.
    - lia.
    - destruct lower, upper; try lia; destruct idx, p; simpl; try apply IHl; try lia; auto.
      set (nth_obligation_1 _ _ _ _ _ _ _).
      set (nth_obligation_1 _ _ _ _ _ _ _).
      clearbody l0.
      clearbody l1.
      assert (l0 = l1) by apply proof_irrelevance.
      now rewrite H3.
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
    induction l; intros; simpl in *.
    - lia.
    - destruct n; simpl; f_equal; auto.
  Qed.

  Theorem nth_map_nth : forall (l : list A) n f H H1, nth (map_nth l n f H) n H1 = f (nth l n H).
  Proof.
    induction l; intros; simpl in *.
    - lia.
    - destruct n; auto.
      simpl.
      rewrite IHl.
      set (nth_obligation_1 _ _ _ _ _ _ _ _).
      set (map_nth_obligation_2 _ _ _ _ _ _ _ _).
      clearbody l0.
      clearbody l1.
      assert (l0 = l1) by apply proof_irrelevance.
      now rewrite H0.
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

Section MapFirstN.
  Context {A : Type}.

  Program Fixpoint map_first_n (l : list A) (n : positive) (f : A -> A) (n_spec : N.pos n <= length l) : list A :=
    match l with
    | [] => _
    | x :: l' =>
      match n with
      | xH => f x :: l'
      | _ => f x :: map_first_n l' (Pos.pred n) f _
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

  Theorem length_map_first_n : forall (l : list A) n f H, length (map_first_n l n f H) = length l.
  Proof.
    induction l.
    - intros.
      simpl in H.
      lia.
    - intros.
      simpl.
      destruct n.
      + simpl.
        set (map_first_n_obligation_2 _ _ _ _ _ _ _ _ _).
        clearbody l0.
        simpl in l0.
        f_equal.
        apply IHl.
      + simpl.
        f_equal.
        apply IHl.
      + auto.
  Qed.

  Theorem nth_error_map_first_n_eq : forall (l : list A) n idx f H, idx < N.pos n -> nth_error (map_first_n l n f H) idx = option_map f (nth_error l idx).
  Proof.
    induction l.
    - intros.
      simpl in H.
      lia.
    - intros.
      simpl.
      destruct n.
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
        destruct idx eqn:E.
        * simpl.
          auto.
        * simpl.
          lia.
  Qed.

  Theorem nth_map_first_n_eq : forall (l : list A) n idx f H H1 H2, idx < N.pos n -> nth (map_first_n l n f H) idx H1 = f (nth l idx H2).
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
    - rewrite nth_error_map_first_n_eq in Heq.
      + destruct (nth_error l idx) eqn:Heq'.
        * simpl in Heq. 
          injection Heq.
          intros.
          auto.
        * apply nth_error_none_length in Heq'.
          lia.
      + auto.
    - destruct (nth_error l idx) eqn:Heq'.
      + apply nth_error_none_length in Heq.
        lia.
      + apply nth_error_none_length in Heq.
        lia.
  Qed.

  Theorem nth_error_map_first_n_neq : forall (l : list A) n idx f H, idx >= N.pos n -> nth_error (map_first_n l n f H) idx = nth_error l idx.
  Proof.
    induction l.
    - intros.
      simpl in H.
      lia.
    - intros.
      simpl.
      destruct n.
      + simpl.
        destruct idx.
        * lia.
        * simpl.
          apply IHl.
          lia.
      + simpl.
        destruct idx.
        * lia.
        * simpl.
          apply IHl.
          lia.
      + simpl.
        destruct idx eqn:E.
        * lia.
        * simpl.
          auto.
  Qed.

  Theorem nth_map_first_n_neq : forall (l : list A) n idx f H H1 H2, idx >= N.pos n -> nth (map_first_n l n f H) idx H1 = nth l idx H2.
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
      + apply nth_error_map_first_n_neq with (l := l) (n := n) (f := f) (H := H) in H0.
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

  Theorem map_first_n_map : forall (l : list A) n f H, length l = N.pos n -> map_first_n l n f H = map f l.
  Proof.
    induction l.
    - intros.
      simpl in H.
      lia.
    - intros.
      simpl.
      destruct n.
      + simpl in H.
        f_equal.
        set (map_first_n_obligation_2 _ _ _ _ _ _ _ _ _).
        clearbody l0.
        simpl in l0.
        apply IHl.
        simpl in H0.
        lia.
      + simpl in H.
        f_equal.
        apply IHl.
        simpl in H.
        simpl in H0.
        lia.
      + f_equal.
        simpl in H0.
        destruct l; auto.
        simpl in H0.
        lia.
  Qed.

  Theorem map_first_id : forall (l : list A) n H, map_first_n l n (fun x => x) H = l.
  Proof.
    induction l.
    - intros.
      simpl in H.
      lia.
    - intros.
      simpl.
      destruct n.
      + auto.
        f_equal.
        set (map_first_n_obligation_2 _ _ _ _ _ _ _ _ _).
        clearbody l0.
        simpl in l0.
        apply IHl.
      + f_equal.
        apply IHl.
      + auto.
  Qed.
End MapFirstN.

Section MapRange.
  Context {A : Type}.

  Program Fixpoint map_range (l : list A) (r : HalfOpenRange.t) (f : A -> A) (r_spec : HalfOpenRange.upper r <= length l) : list A :=
    match l with
    | [] => _
    | x :: l' =>
      match HalfOpenRange.lower r =? 0 with
      | true => map_first_n l (HalfOpenRange.length r) f _
      | false => x :: map_range l' (HalfOpenRange.shift_minus r 1 _) f _
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

  Theorem length_map_range : forall (l : list A) r f H, length (map_range l r f H) = length l.
  Proof.
    induction l.
    - intros.
      assert (HalfOpenRange.upper r > 0) by apply HalfOpenRange.upper_is_positive.
      simpl in H.
      lia.
    - intros.
      simpl.
      set (map_first_n_obligation_2 _ _ _ _ _ _).
      clearbody l0.
      set (map_range_obligation_3 _ _ _ _ _ _).
      clearbody l1.
      simpl in l1.
      set (map_range_obligation_4 _ _ _ _ _ _).
      clearbody l2.
      simpl in l2.
      destruct (HalfOpenRange.lower r =? 0) eqn:E.
      + destruct (HalfOpenRange.length r) eqn:E'.
        * simpl.
          f_equal.
          apply length_map_first_n.
        * simpl.
          f_equal.
          apply length_map_first_n.
        * auto.
      + simpl.
        f_equal.
        apply IHl.
  Qed.

  Theorem lower_0_map_range_map_first_n : forall (l : list A) r f H H1, (HalfOpenRange.lower r = 0) -> map_range l r f H = map_first_n l (HalfOpenRange.length r) f H1.
  Proof.
    induction l.
    - intros.
      assert (HalfOpenRange.upper r > 0) by apply HalfOpenRange.upper_is_positive.
      simpl in H1.
      lia.
    - intros.
      simpl.
      set (map_first_n_obligation_2 _ _ _ _ _ _).
      clearbody l0.
      simpl in l0.
      set (map_range_obligation_3 _ _ _ _ _ _).
      clearbody l1.
      simpl in l1.
      set (map_range_obligation_4 _ _ _ _ _ _).
      clearbody l2.
      simpl in l2.
      set (map_first_n_obligation_2 _ _ _ _ _ _).
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

  Theorem nth_error_map_range : forall (l : list A) r f idx H, HalfOpenRange.contains r idx -> nth_error (map_range l r f H) idx = option_map f (nth_error l idx).
  Proof.
    induction l.
    - intros.
      assert (HalfOpenRange.upper r > 0) by apply HalfOpenRange.upper_is_positive.
      simpl in H.
      lia.
    - intros.
      unfold map_range.
      set (map_range_obligation_2 _ _ _ _ _ _).
      clearbody l0.
      simpl in l0.
      set (map_range_obligation_3 _ _ _ _ _ _).
      clearbody l1.
      simpl in l1.
      set (map_range_obligation_4 _ _ _ _ _ _).
      clearbody l2.
      simpl in l2.
      destruct (HalfOpenRange.lower r =? 0) eqn:E.
      + apply nth_error_map_first_n_eq.
        simpl in H.
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

  Theorem nth_map_range : forall (l : list A) r f idx H H1 H2, HalfOpenRange.contains r idx -> nth (map_range l r f H) idx H1 = f (nth l idx H2).
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
    - rewrite nth_error_map_range in Heq.
      + destruct (nth_error l idx) eqn:Heq'.
        * simpl in Heq. 
          injection Heq.
          intros.
          auto.
        * apply nth_error_none_length in Heq'.
          lia.
      + auto.
    - destruct (nth_error l idx) eqn:Heq'.
      + apply nth_error_none_length in Heq.
        lia.
      + apply nth_error_none_length in Heq.
        lia.
  Qed.

  Theorem map_range_map : forall (l : list A) r f H, HalfOpenRange.lower r = 0 -> HalfOpenRange.upper r = length l -> map_range l r f H = map f l.
  Proof.
    induction l.
    - intros.
      assert (HalfOpenRange.upper r > 0) by apply HalfOpenRange.upper_is_positive.
      simpl in H.
      lia.
    - intros.
      unfold map_range.
      set (map_range_obligation_2 _ _ _ _ _ _).
      clearbody l0.
      simpl in l0.
      set (map_range_obligation_3 _ _ _ _ _ _).
      clearbody l1.
      simpl in l1.
      set (map_range_obligation_4 _ _ _ _ _ _).
      clearbody l2.
      simpl in l2.
      destruct (HalfOpenRange.lower r =? 0) eqn:E.
      + apply map_first_n_map.
        rewrite N.eqb_eq in E.
        simpl.
        rewrite HalfOpenRange.lower_0_length_eq_upper; auto.
      + rewrite N.eqb_neq in E.
        lia.
  Qed.

  Theorem nth_error_map_range_eq : forall (l : list A) r f idx H, HalfOpenRange.contains r idx -> nth_error (map_range l r f H) idx = option_map f (nth_error l idx).
  Proof.
    induction l.
    - intros.
      assert (HalfOpenRange.upper r > 0) by apply HalfOpenRange.upper_is_positive.
      simpl in H.
      lia.
    - intros.
      unfold map_range.
      set (map_range_obligation_2 _ _ _ _ _ _).
      clearbody l0.
      simpl in l0.
      set (map_range_obligation_3 _ _ _ _ _ _).
      clearbody l1.
      simpl in l1.
      set (map_range_obligation_4 _ _ _ _ _ _).
      clearbody l2.
      simpl in l2.
      destruct (HalfOpenRange.lower r =? 0) eqn:E.
      + apply nth_error_map_first_n_eq.
        simpl in H.
        unfold HalfOpenRange.contains in H0.
        destruct H0.
        rewrite HalfOpenRange.lower_0_length_eq_upper; auto.
        rewrite N.eqb_eq in E.
        auto.
      + rewrite N.eqb_neq in E.
        unfold HalfOpenRange.contains in H0.
        simpl.
        destruct idx eqn:E'.
        * destruct H0.
          lia.
        * apply IHl. 
          simpl.
          unfold HalfOpenRange.contains.
          split.
          -- simpl.
             lia.
          -- simpl.
             lia.
  Qed.

  Theorem nth_map_range_eq : forall (l : list A) r f idx H H1 H2, HalfOpenRange.contains r idx -> nth (map_range l r f H) idx H1 = f (nth l idx H2).
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
    - rewrite nth_error_map_range_eq in Heq.
      + destruct (nth_error l idx) eqn:Heq'.
        * simpl in Heq. 
          injection Heq.
          intros.
          auto.
        * apply nth_error_none_length in Heq'.
          lia.
      + auto.
    - destruct (nth_error l idx) eqn:Heq'.
      + apply nth_error_none_length in Heq.
        lia.
      + apply nth_error_none_length in Heq.
        lia.
  Qed.

  Theorem nth_error_map_range_neq : forall (l : list A) r f idx H, ~ (HalfOpenRange.contains r idx) -> nth_error (map_range l r f H) idx = nth_error l idx.
  Proof.
    induction l.
    - intros.
      assert (HalfOpenRange.upper r > 0) by apply HalfOpenRange.upper_is_positive.
      simpl in H.
      lia.
    - intros.
      unfold map_range.
      set (map_range_obligation_2 _ _ _ _ _ _).
      clearbody l0.
      simpl in l0.
      set (map_range_obligation_3 _ _ _ _ _ _).
      clearbody l1.
      simpl in l1.
      set (map_range_obligation_4 _ _ _ _ _ _).
      clearbody l2.
      simpl in l2.
      destruct (HalfOpenRange.lower r =? 0) eqn:E.
      + apply nth_error_map_first_n_neq.
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

  Theorem nth_map_range_neq : forall (l : list A) r f idx H H1 H2, ~ (HalfOpenRange.contains r idx) -> nth (map_range l r f H) idx H1 = nth l idx H2.
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
      + apply nth_error_map_range_neq with (l := l) (r := r) (f := f) (H := H) in H0.
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
End MapRange.

Create HintDb list.

Hint Unfold length
            repeat_pos
            repeat
            nth_error
            nth
            update
            update_range 
            map_nth
            : list.

Hint Resolve app_length
             length_zero_iff_nil

             length_repeat_pos
             repeat_pos_in
             repeat_pos_non_empty
             
             length_repeat
             repeat_in
             repeat_empty_iff_zero

             nth_in

             length_update
             update_in
             nth_update_eq
             nth_update_neq

             length_update_range
             nth_update_range_eq
             nth_update_range_neq

             length_map_nth
             nth_map_nth
             : list.
