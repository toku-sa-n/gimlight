Set Default Goal Selector "!".

From Coq Require Import ZArith.
From Coq Require Import Lia.
From Coquill Require NonEmptyArray.
From Coquill Require Import PArith.

Open Scope Z_scope.

Definition t (width height : positive) : Type := NonEmptyArray.t (NonEmptyArray.t bool width) height.

Definition all_wall_map (width height : positive) : t width height :=
  NonEmptyArray.repeat (NonEmptyArray.repeat true width) height.
