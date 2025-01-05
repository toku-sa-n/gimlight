Set Default Goal Selector "!".

From Coq Require Import NArith.

From GameLogic Require Import Coord.
From GameLogic Require Import Map.

Definition in_bounds {width height} (map : t width height) (p : Coord.t) : Prop :=
  match p with
  | (x, y) => x < N.pos width /\ y < N.pos height
  end.

Definition can_move {width height} (map : t width height) (src dst : Coord.t) H1 H2 : Prop :=
  Coord.adjacent src dst /\ get_at dst H1 H2 map = false.

Inductive reachable {width height} (map : t width height) : Coord.t -> Coord.t -> Prop :=
  | reachable_refl : forall p (H1 : in_bounds map p), reachable map p p
  | reachable_move :
      forall src dst mid H1 H2,
        reachable map src mid ->
        can_move map mid dst H1 H2 ->
        reachable map src dst.

Definition connected {width height} (map : t width height) : Prop :=
  forall p1 p2 (H1 : in_bounds map p1) (H2 : in_bounds map p2),
    reachable map p1 p2.
