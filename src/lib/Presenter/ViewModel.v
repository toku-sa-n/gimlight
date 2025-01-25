Set Default Goal Selector "!".

From Coq Require Import PArith.
From Coq Require Import NArith.

From Coquill Require CArray.

#[local]
Open Scope positive_scope.

Definition t := CArray.t (CArray.t bool (N.pos 100)) (N.pos 100).
