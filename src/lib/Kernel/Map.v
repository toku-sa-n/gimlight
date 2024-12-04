Set Default Goal Selector "!".

From Coq Require Import ZArith.
From Coq Require Import Lia.

From Coquill Require Import PArith.

From Coquill Require HalfOpenRange.
From Coquill Require NonEmptyArray.

Open Scope Z_scope.

Definition t (width height : positive) : Type := NonEmptyArray.t (NonEmptyArray.t bool width) height.

Definition all_wall_map (width height : positive) : t width height :=
  NonEmptyArray.repeat (NonEmptyArray.repeat true width) height.

