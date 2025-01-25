(* We cannot use the name `Array` because it conflicts with OCaml's `Array`
 * module when extracting.
 *
 * `C` stands for Coq or Coquill. *)

Set Default Goal Selector "!".

From Coq Require Import NArith.
From Coq Require Import Classes.SetoidClass.
From Coq Require Import extraction.Extraction.
From Coq Require Import Lia.
From Coq Require Import ProofIrrelevance.

Open Scope N_scope.

Inductive t (A : Type) : Type :=
  | empty : t A
  | cons : A -> t A -> t A.

Arguments empty {A}.
Arguments cons {A} _ _.

Declare Scope array_scope.

Infix ":|:" := cons (at level 60, right associativity) : array_scope.
Delimit Scope array_scope with array.

#[local]
Open Scope array_scope.

Notation "[| |]" := empty : array_scope.
Notation "[| x |]" := (cons x empty) : array_scope.
Notation "[| x ; .. ; y |]" := (cons x .. (cons y empty) ..) : array_scope.

Section Length.
  Context {A : Type}.

  Fixpoint length (xs : t A) : N :=
    match xs with
    | empty => 0
    | _ :|: xs' => N.succ (length xs')
    end.
End Length.

Section Append.
  Context {A : Type}.

  Fixpoint append (xs : t A) (ys : t A) : t A :=
    match xs with
    | empty => ys
    | x :|: xs' => x :|: append xs' ys
    end.
  
  Theorem length_append : forall (xs ys : t A), length (append xs ys) = length xs + length ys.
  Proof.
    intros xs ys.
    induction xs as [ | h t IH]; simpl; lia.
  Qed.
End Append.

Section MakeNonempty.
  Context {A : Type}.

  Fixpoint make_nonempty (n : positive) (x : A) : t A :=
    match n with
    | xH => [| x |]
    | xO n' => append (make_nonempty n' x) (make_nonempty n' x)
    | xI n' => x :|: append (make_nonempty n' x) (make_nonempty n' x)
    end.

  Theorem length_make_nonempty : forall (n : positive) (x : A), length (make_nonempty n x) = N.pos n.
  Proof.
    intros n x.
    induction n as [n' IHn' | n' IHn' | ];
      simpl; try (rewrite length_append; rewrite IHn'); lia.
  Qed.
End MakeNonempty.

Section Make.
  Context {A : Type}.

  Definition make {A : Type} (n : N) (x : A) : t A :=
    match n with
    | 0 => empty
    | Npos n' => make_nonempty n' x
    end.

  Theorem make_0_eq_empty : forall {A : Type} (x : A), make 0 x = [| |].
  Proof.
    reflexivity.
  Qed.

  Theorem length_make : forall (n : N) (x : A), length (make n x) = n.
  Proof.
    intros n x.
    destruct n; try apply length_make_nonempty.
    reflexivity.
  Qed.
End Make.

Section MakeMatrix.
  Context {A : Type}.

  Definition make_matrix {A : Type} (width height : N) (x : A) : t (t A) :=
    make width (make height x).
End MakeMatrix.

Section MapNth.
  Context {A : Type}.

  Program Fixpoint map_nth (xs : t A) (i : N) (f : A -> A) (H : i < length xs)  : t A :=
    match xs, i with
    | [| |], _ => _
    | x :|: xs', 0 => f x :|: xs'
    | x :|: xs', (N.pos _) => x :|: map_nth xs' (N.pred i) f _
    end.
  Next Obligation.
  Proof.
    simpl in H.
    lia.
  Qed.
  Next Obligation.
  Proof.
    simpl in H.
    lia.
  Qed.

  Theorem length_map_nth : forall (xs : t A) (i : N) (f : A -> A) (H : i < length xs),
    length (map_nth xs i f H) = length xs.
  Proof.
    induction xs as [ | h t IHxs]; intros i f H; simpl in *; try lia.
    destruct i; simpl; auto.
    now rewrite IHxs.
  Qed.

  Theorem map_nth_id : forall (xs : t A) (i : N) (H : i < length xs), map_nth xs i (fun x => x) H = xs.
  Proof.
    induction xs as [ | h t IHxs]; intros i H; simpl in *; try lia; destruct i; f_equal; apply IHxs; lia.
  Qed.
End MapNth.

Section MapRange.
  Context {A : Type}.

  Program Fixpoint map_range (xs : t A) (from to : N) (f : A -> A) (H : from < to <= length xs) : t A :=
    match xs, from, to with
    | [| |], _, _ => _
    | x :|: xs', 0, 0 => _
    | x :|: xs', 0, N.pos xH => f x :|: xs'
    | x :|: xs', 0, N.pos (xO _)
    | x :|: xs', 0, N.pos (xI _) => f x :|: map_range xs' 0 (N.pred to) f _
    | x :|: xs', N.pos _, 0 => _
    | x :|: xs', N.pos _, N.pos _ => x :|: map_range xs' (N.pred from) (N.pred to) f _
    end.
  Next Obligation.
  Proof.
    simpl in *.
    lia.
  Qed.
  Next Obligation.
  Proof.
    simpl in *.
    lia.
  Qed.
  Next Obligation.
  Proof.
    simpl in *.
    lia.
  Qed.
  Next Obligation.
  Proof.
    simpl in *.
    lia.
  Qed.

  Theorem length_map_range : forall (xs : t A) from to f H,
    length (map_range xs from to f H) = length xs.
  Proof.
    induction xs as [ | h t IHxs]; intros from to f H; simpl in *; try lia.
    destruct from as [ | from'], to as [ | to'];
      try lia;
      try (destruct to' as [to' | to' | ]; auto);
      simpl; now f_equal.
  Qed.

  Theorem map_range_id : forall (xs : t A) from to H, map_range xs from to (fun x => x) H = xs.
  Proof.
    induction xs as [ | h t IHxs]; intros from to H; simpl in *; try lia.
    destruct from as [ | from'], to as [ | to']; try easy.
    + destruct to' as [to' | to' | ]; auto; rewrite IHxs; auto.
    + rewrite IHxs.
      auto.
  Qed.

  Theorem map_range_eq_map_nth : forall (xs : t A) idx f H H1,
    map_range xs idx (idx + 1) f H = map_nth xs idx f H1.
  Proof.
    intros xs idx.
    remember (idx + 1) as to.
    generalize dependent idx.
    generalize dependent to.
    induction xs as [ | h t IHxs]; intros to idx H H1; simpl in *; try lia.
    destruct idx as [ | idx'], to as [ | to']; try lia; simpl in *.
    - assert (H0 : to' = 1%positive) by lia.
      rewrite H0.
      auto.
    - intros H0 H2.
      erewrite IHxs; try lia.
      f_equal.
  Qed.
End MapRange.

Section UpdateNth.
  Context {A : Type}.

  Definition update_nth (xs : t A) (i : N) (y : A) (H : i < length xs) : t A :=
    map_nth xs i (fun _ => y) H.

  Theorem update_nth_eq_map_nth : forall xs idx y H,
    update_nth xs idx y H = map_nth xs idx (fun _ => y) H.
  Proof.
    easy.
  Qed.

  Theorem length_update_nth : forall (xs : t A) (i : N) (y : A) (H : i < length xs),
    length (update_nth xs i y H) = length xs.
  Proof.
    intros.
    apply length_map_nth.
  Qed.
End UpdateNth.

Section UpdateRange.
  Context {A : Type}.

  Definition update_range (xs : t A) (from to : N) (y : A) (H : from < to <= length xs) : t A :=
    map_range xs from to (fun _ => y) H.

  Theorem length_update_range : forall (xs : t A) from to y H,
    length (update_range xs from to y H) = length xs.
  Proof.
    intros.
    apply length_map_range.
  Qed.

  Theorem update_range_eq_map_range : forall (xs : t A) from to y H,
    update_range xs from to y H = map_range xs from to (fun _ => y) H.
  Proof.
    easy.
  Qed.

  Theorem update_range_eq_update_nth : forall (xs : t A) idx y H H1,
    update_range xs idx (idx + 1) y H = update_nth xs idx y H1.
  Proof.
    intros.
    unfold update_range, update_nth.
    apply map_range_eq_map_nth.
  Qed.
End UpdateRange.

Extract Inductive t => "array" ["[||]" "(fun (_, x, xs) -> Array.append [|x|] xs)"]
    "(fun f_nil f_cons xs -> if xs = [||] then f_nil () else f_cons (Array.length xs - 1 |> Z.of_int) xs.(0) (Array.sub xs 1 (Array.length xs - 1)))".

Extract Constant append => "Array.append".
Extract Constant make => "(fun n x -> Array.make (Z.to_int n) x)".
Extract Constant make_matrix => "(fun width height x -> Array.make_matrix (Z.to_int width) (Z.to_int height) x)".
