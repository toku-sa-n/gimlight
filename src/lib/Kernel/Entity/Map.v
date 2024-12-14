Set Default Goal Selector "!".

From Coq Require Import FunctionalExtensionality.
From Coq Require Import ProofIrrelevance.
From Coq Require Import ZArith.
From Coq Require Import Lia.

From Coquill Require Import FixedSizeArray.

Open Scope N_scope.

Definition t (width height : positive) : Type := FixedSizeArray.t (FixedSizeArray.t bool (N.pos height)) (N.pos width).

Definition all_wall_map (width height : positive) : t width height :=
  FixedSizeArray.make_matrix (N.pos width) (N.pos height) true.

Program Definition build_horizontal_road
  {width height : positive}
  (x_from x_to : N)
  (y : N)
  (x_spec : x_from < x_to <= N.pos width)
  (y_spec : y < N.pos height)
  (map : t width height) : t width height :=
  FixedSizeArray.map_range x_from x_to (FixedSizeArray.update_nth y false _) _ map.

Program Definition build_vertical_road
  {width height : positive}
  (x : N)
  (y_from y_to : N)
  (x_spec : x < N.pos width) 
  (y_spec : y_from < y_to <= N.pos height)
  (map : t width height) : t width height :=
  FixedSizeArray.map_nth x (FixedSizeArray.update_range y_from y_to false _) _ map.

Program Definition build_room
  {width height : positive}
  (x_from x_to y_from y_to : N)
  (x_spec : x_from < x_to <= N.pos width)
  (y_spec : y_from < y_to <= N.pos height)
  (map : t width height) : t width height :=
  FixedSizeArray.map_range x_from x_to (FixedSizeArray.update_range y_from y_to false _) _ map.

Theorem build_room_eq_build_horizontal_road {width height : positive} x_from x_to y H1 H2 H3 H4 (map : t width height) :
  build_room x_from x_to y (y + 1) H1 H2 map = build_horizontal_road x_from x_to y H3 H4 map.
Proof.
  unfold build_room, build_horizontal_road.
  set (build_room_obligation_1 _ _ _ _ _ _ _ _).
  set (build_room_obligation_2 _ _ _ _ _ _ _ _).
  set (build_horizontal_road_obligation_1 _ _ _ _ _ _ _).
  set (build_horizontal_road_obligation_2 _ _ _ _).
  clearbody a a0 l a1.
  f_equal.
  - apply functional_extensionality.
    intros.
    apply update_range_eq_update_nth.
  - apply proof_irrelevance.
Qed.

Theorem build_room_eq_build_vertical_road {width height : positive} x y_from y_to H1 H2 H3 H4 (map : t width height) :
  build_room x (x + 1) y_from y_to H1 H2 map = build_vertical_road x y_from y_to H3 H4 map.
Proof.
  unfold build_room, build_vertical_road.
  set (build_room_obligation_1 _ _ _ _ _ _ _ _).
  set (build_room_obligation_2 _ _ _ _ _ _ _ _).
  set (build_vertical_road_obligation_1 _ _ _ _).
  set (build_vertical_road_obligation_2 _ _ _ _ _ _ _).
  clearbody a a0 l a1.
  erewrite map_range_eq_map_nth.
  f_equal.
  apply functional_extensionality.
  intros.
  assert (a = a1) by apply proof_irrelevance.
  now rewrite H.
Qed.

Program Definition initial_map : t 80 50 :=
  build_vertical_road 40 10 30 _ _ (build_horizontal_road 10 50 25 _ _ (all_wall_map 80 50)).
Next Obligation.
Proof.
  lia.
Qed.
Next Obligation.
Proof.
  lia.
Qed.
Next Obligation.
Proof.
  lia.
Qed.
Next Obligation.
Proof.
  lia.
Qed.
