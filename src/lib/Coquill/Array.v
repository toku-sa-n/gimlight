Set Default Goal Selector "!".

From Coq Require Import NArith.
From Coq Require Import Classes.SetoidClass.
From Coq Require Import extraction.Extraction.
From Coq Require Import Lia.

Open Scope N_scope.

Inductive t (A : Type) : N -> Type :=
  | empty : t A 0
  | cons : forall {n : N}, A -> t A n -> t A (n + 1).

Arguments empty {A}.
Arguments cons {A n} _ _.

Program Fixpoint append {A : Type} {n m : N} (xs : t A n) (ys : t A m) : t A (n + m) :=
  match xs with
  | empty => ys
  | cons x xs' => cons x (append xs' ys)
  end.
Next Obligation.
Proof.
  apply N.add_shuffle0.
Qed.

Program Fixpoint make_nonempty {A : Type} (n : positive) (x : A) : t A (N.pos n) :=
  match n with
  | xH => cons x empty
  | xO n' => append (make_nonempty n' x) (make_nonempty n' x)
  | xI n' => cons x (append (make_nonempty n' x) (make_nonempty n' x))
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

Definition make {A : Type} (n : N) (x : A) : t A n :=
  match n with
  | 0 => empty
  | Npos n' => make_nonempty n' x
  end.

Theorem make_0_eq_empty : forall {A : Type} (x : A), make 0 x = empty.
Proof.
  reflexivity.
Qed.

Definition make_matrix {A : Type} (width height : N) (x : A) : t (t A height) width :=
  make width (make height x).

Program Fixpoint map_nth {A : Type} {n : N} (i : N) (f : A -> A) (H : i < n) (xs : t A n) : t A n :=
  match xs, i with
  | empty, _ => _
  | cons x xs', 0 => cons (f x) xs'
  | cons x xs', (N.pos _) => cons x (map_nth (N.pred i) f _ xs')
  end.
Next Obligation.
Proof.
  rewrite N.add_1_r in H.
  destruct wildcard'0; lia.
Qed.
