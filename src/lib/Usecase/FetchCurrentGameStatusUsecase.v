Set Default Goal Selector "!".

From Coq Require Import PArith.
From Coq Require Import NArith.

From Coquill Require CArray.
From Coquill Require IO.

Inductive output (width height : positive) :=
  | current_map : CArray.t (CArray.t bool (N.pos height)) (N.pos width) -> output width height.

Arguments current_map {width height} _.

Structure t (width height : positive) := {
  execute : unit -> IO.t (output width height);
}.

Arguments execute {width height} _.
