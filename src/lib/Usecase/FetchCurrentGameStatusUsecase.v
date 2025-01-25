Set Default Goal Selector "!".

From Coq Require Import PArith.
From Coq Require Import NArith.

From Coquill Require CArray.
From Coquill Require IO.

Inductive output :=
  | current_map : CArray.t (CArray.t bool) -> output.

Structure t := {
  execute : unit -> IO.t output;
}.
