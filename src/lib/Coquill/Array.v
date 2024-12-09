Set Default Goal Selector "!". 

From Coq Require Import NArith.

Open Scope N_scope.

Inductive t (A : Type) : N -> Type :=
  | empty : t A 0
  | cons : forall {n : N}, A -> t A n -> t A (n + 1).

Arguments empty {A}.
Arguments cons {A n} _ _.



