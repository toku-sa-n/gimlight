Set Default Goal Selector "!".

From Coq Require Import PArith.
From Coq Require Import NArith.

From Coquill Require FixedSizeArray.

#[local]
Open Scope positive_scope.

Definition t := FixedSizeArray.t (FixedSizeArray.t bool (N.pos 100)) (N.pos 100).
