Set Default Goal Selector "!".

From Coq Require Import NArith.

From Entity Require Import Coord.
From Entity Require Import Map.

Definition in_bounds {width height} (map : t width height) (p : Coord.t) : Prop :=
  match p with
  | (x, y) => x < N.pos width /\ y < N.pos height
  end.


Definition adjacent (p1 p2 : Coord.t) : Prop :=
  match p1, p2 with
  | (x1, y1), (x2, y2) =>
    (x1 = x2 /\ y1 = y2 + 1) \/
    (x1 = x2 /\ y1 = y2 - 1) \/
    (x1 = x2 + 1 /\ y1 = y2) \/
    (x1 = x2 - 1 /\ y1 = y2)
  end.

Definition can_move {width height} (map : t width height) (src dst : Coord.t) H1 H2 : Prop :=
  adjacent src dst /\ get_at dst H1 H2 map = false.

Inductive reachable {width height} (map : t width height) : Coord.t -> Coord.t -> Prop :=
  | reachable_refl : forall p (H1 : in_bounds map p), reachable map p p
  | reachable_move :
      forall src dst mid H1 H2,
        reachable map src mid ->
        can_move map mid dst H1 H2 ->
        reachable map src dst.
