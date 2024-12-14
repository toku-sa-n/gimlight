Set Default Goal Selector "!".

From Coq Require Import NArith.
From Coq Require Import PArith.

From Coquill Require Import FixedSizeArray.

From Repository Require Import MapRepository.

Inductive t (width height : positive) :=
  | make : MapRepository.t width height -> t width height.

Arguments make {width height} _.

Inductive output (width height : positive) :=
  | initial_map : FixedSizeArray.t (FixedSizeArray.t bool (N.pos height)) (N.pos width) -> output width height.

Arguments initial_map {width height} _.

Definition execute {width height : positive} (r : t width height) : output width height :=
  match r with
  | make repository => initial_map (MapRepository.get repository tt)
  end.

