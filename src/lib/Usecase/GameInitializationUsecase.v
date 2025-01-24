Set Default Goal Selector "!".

From Coq Require Import PArith.
From Coq Require Import NArith.

From Coquill Require FixedSizeArray.
From Coquill Require IO.

Inductive output (width height : positive) :=
  | initial_map : FixedSizeArray.t (FixedSizeArray.t bool (N.pos height)) (N.pos width) -> output width height.

Arguments initial_map {width height} _.

Structure t (width height : positive) := {
  execute : unit -> IO.t (output width height);
}.

Arguments execute {width height} _.
