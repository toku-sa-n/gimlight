Set Default Goal Selector "!".

From Coquill Require Array.
From Coquill Require Import List.
From Coquill Require Import PArith.
From Coquill Require Import NArith.

Open Scope N_scope.

Definition t (A : Type) (n : positive) : Type := Array.t A (Npos n).

Definition make {A : Type} {n : positive} (data : Array.t A (Npos n)) : t A n := data.

Program Definition repeat {A : Type} (x : A) (n : positive) : t A n :=
  make (Array.repeat x (Npos n)).

Section UpdateRange.
  Context {A : Type}.
  Context {n : positive}.

  Program Definition update_range (arr : t A n) (r : HalfOpenRange.t) (x : A) (upper_spec : HalfOpenRange.upper r <= N.pos n) : t A n :=
    make (Array.update_range arr r x _).
End UpdateRange.
