(* We cannot use the name `Array` because it conflicts with OCaml's `Array`
 * module when extracting. *)

Set Default Goal Selector "!".

From Coq Require Import NArith.
From Coq Require Import Classes.SetoidClass.
From Coq Require Import extraction.Extraction.
From Coq Require Import Lia.
From Coq Require Import ProofIrrelevance.

Open Scope N_scope.

Inductive t (A : Type) : N -> Type :=
  | empty : t A 0
  | cons : forall {n : N}, A -> t A n -> t A (n + 1).

Arguments empty {A}.
Arguments cons {A n} _ _.

Declare Scope array_scope.

Infix ":|:" := cons (at level 60, right associativity) : array_scope.
Delimit Scope array_scope with array.

Local Open Scope array_scope.

Notation "[| |]" := empty : array_scope.
Notation "[| x |]" := (cons x empty) : array_scope.
Notation "[| x ; .. ; y |]" := (cons x .. (cons y empty) ..) : array_scope.

Section Append.
  Context {A : Type}.

  Program Fixpoint append {n m : N} (xs : t A n) (ys : t A m) : t A (n + m) :=
    match xs with
    | empty => ys
    | x :|: xs' => x :|: append xs' ys
    end.
  Next Obligation.
  Proof.
    apply N.add_shuffle0.
  Qed.
End Append.

Section MakeNonempty.
  Context {A : Type}.

  Program Fixpoint make_nonempty (n : positive) (x : A) : t A (N.pos n) :=
    match n with
    | xH => [| x |]
    | xO n' => append (make_nonempty n' x) (make_nonempty n' x)
    | xI n' => x :|: append (make_nonempty n' x) (make_nonempty n' x)
    end.
  Next Obligation.
  Proof.
    f_equal.
    apply Pos.add_diag.
  Qed.
  Next Obligation.
  Proof.
    f_equal.
    now rewrite Pos.add_diag.
  Qed.
End MakeNonempty.

Section Make.
  Context {A : Type}.

  Definition make {A : Type} (n : N) (x : A) : t A n :=
    match n with
    | 0 => empty
    | Npos n' => make_nonempty n' x
    end.

  Theorem make_0_eq_empty : forall {A : Type} (x : A), make 0 x = [| |].
  Proof.
    reflexivity.
  Qed.
End Make.

Section MakeMatrix.
  Context {A : Type}.

  Definition make_matrix {A : Type} (width height : N) (x : A) : t (t A height) width :=
    make width (make height x).
End MakeMatrix.

Section Get.
  Context {A : Type}.

  Program Fixpoint get {n : N} (i : N) (H : i < n) (xs : t A n) : A :=
    match xs, i with
    | [| |], _ => _
    | x :|: _, 0 => x
    | _ :|: xs', N.pos i' => get (N.pred i) _ xs'
    end.
  Next Obligation.
  Proof.
    lia.
  Qed.
  Next Obligation.
  Proof.
    lia.
  Qed.
End Get.

Section MapNth.
  Context {A : Type}.

  Program Fixpoint map_nth {n : N} (i : N) (f : A -> A) (H : i < n) (xs : t A n) : t A n :=
    match xs, i with
    | [| |], _ => _
    | x :|: xs', 0 => f x :|: xs'
    | x :|: xs', (N.pos _) => x :|: map_nth (N.pred i) f _ xs'
    end.
  Next Obligation.
  Proof.
    lia.
  Qed.

  Theorem map_nth_id : forall {n : N} (i : N) (H : i < n) (xs : t A n), map_nth i (fun x => x) H xs = xs.
  Proof.
    intros.
    generalize dependent i.
    induction xs; intros; try lia.
    simpl.
    destruct i; f_equal; easy.
  Qed.

  Theorem map_nth_eq_get : forall {n : N} (i : N) f H (xs : t A n),
    get i H (map_nth i f H xs) = f (get i H xs).
  Proof.
    intros.
    generalize dependent i.
    generalize dependent f.
    induction xs; intros; try lia.
    simpl.
    destruct i; auto.
    simpl.
    set (map_nth_obligation_2 _ _ _ _ _ _ _ _ _ _ _).
    set (get_obligation_2 _ _ _ _ _ _ _ _ _ _ _).
    set (get_obligation_2 _ _ _ _ _ _ _ _ _ _ _).
    clearbody l l0 l1.
    assert (l = l1) by apply proof_irrelevance.
    assert (l0 = l1) by apply proof_irrelevance.
    now rewrite H0, H1.
  Qed.
End MapNth.

Section MapRange.
  Context {A : Type}.

  Program Fixpoint map_range {n : N} (from to : N) (f : A -> A) (H : from < to <= n) (xs : t A n) : t A n :=
    match xs, from, to with
    | [| |], _, _ => _
    | x :|: xs', 0, 0 => _
    | x :|: xs', 0, N.pos xH => f x :|: xs'
    | x :|: xs', 0, N.pos (xO _)
    | x :|: xs', 0, N.pos (xI _) => f x :|: map_range 0 (N.pred to) f _ xs'
    | x :|: xs', N.pos _, 0 => _
    | x :|: xs', N.pos _, N.pos _ => x :|: map_range (N.pred from) (N.pred to) f _ xs'
    end.
  Next Obligation.
  Proof.
    lia.
  Qed.
  Next Obligation.
  Proof.
    lia.
  Qed.
  Next Obligation.
  Proof.
    lia.
  Qed.

  Theorem map_range_id : forall {n : N} from to H (xs : t A n), map_range from to (fun x => x) H xs = xs.
  Proof.
    intros.
    generalize dependent from.
    generalize dependent to.
    induction xs; intros; try lia.
    simpl.
    destruct from, to; try destruct p; now f_equal.
  Qed.

  Theorem map_range_eq_map_nth : forall {n : N} idx f H H1 (xs : t A n),
    map_range idx (idx + 1) f H xs = map_nth idx f H1 xs.
  Proof.
    intros.
    remember (idx + 1) as to.
    generalize dependent idx.
    generalize dependent to.
    induction xs; intros; try lia.
    simpl.
    destruct idx, to; try destruct p; try easy; f_equal; apply IHxs; lia.
  Qed.
End MapRange.

Section UpdateNth.
  Context {A : Type}.

  Definition update_nth {n : N} (i : N) (y : A) (H : i < n) (xs : t A n) : t A n :=
    map_nth i (fun _ => y) H xs.

  Theorem update_nth_eq_map_nth : forall {n : N} idx y H (xs : t A n),
    update_nth idx y H xs = map_nth idx (fun _ => y) H xs.
  Proof.
    easy.
  Qed.

  Theorem update_nth_eq_get : forall {n : N} (i : N) y H (xs : t A n),
    get i H (update_nth i y H xs) = y.
  Proof.
    intros.
    unfold update_nth.
    apply map_nth_eq_get.
  Qed.
End UpdateNth.

Section UpdateRange.
  Context {A : Type}.

  Definition update_range {n : N} (from to : N) (y : A) (H : from < to <= n) (xs : t A n) : t A n :=
    map_range from to (fun _ => y) H xs.

  Theorem update_range_eq_map_range : forall {n : N} from to y H (xs : t A n),
    update_range from to y H xs = map_range from to (fun _ => y) H xs.
  Proof.
    easy.
  Qed.

  Theorem update_range_eq_update_nth : forall {n : N} idx y H H1 (xs : t A n),
    update_range idx (idx + 1) y H xs = update_nth idx y H1 xs.
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
Extract Constant get => "(fun i xs -> Array.get (Z.to_int i) xs)".
