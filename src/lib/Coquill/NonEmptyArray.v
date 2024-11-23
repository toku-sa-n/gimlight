Set Default Goal Selector "!".

From Coquill Require Array.
From Coquill Require Import List.
From Coquill Require Import PArith.

Definition t (A : Type) (n : positive) : Type := Array.t A (Npos n).

Definition make {A : Type} (n : positive) (data : Array.t A (Npos n)) : t A n := data.

Program Definition repeat {A : Type} (x : A) (n : positive) : t A n :=
  make n (Array.repeat x (Npos n)).
