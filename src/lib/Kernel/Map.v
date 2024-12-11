Set Default Goal Selector "!".

From Coq Require Import ZArith.
From Coq Require Import Lia.

From Coquill Require Import Array.

Open Scope N_scope.

Definition t (width height : positive) : Type := Array.t (Array.t bool (N.pos height)) (N.pos width).

Definition all_wall_map (width height : positive) : t width height :=
  Array.make_matrix (N.pos width) (N.pos height) true.

Program Definition build_horizontal_road
  {width height : positive}
  (x_from x_to : N)
  (y : N)
  (map : t width height)
  (x_spec : x_from < x_to <= N.pos width)
  (y_spec : y < N.pos height) : t width height :=
  NonEmptyArray.map_nth map y (fun row => NonEmptyArray.update_range row x false x_spec) _.

Program Definition build_vertical_road
  {width height : positive}
  (x : N)
  (y : HalfOpenRange.t)
  (map : t width height)
  (x_spec : x < N.pos width) 
  (y_spec : HalfOpenRange.upper y <= N.pos height) : t width height :=
  NonEmptyArray.map_range map y (fun row => NonEmptyArray.update row x false _) _.

Program Definition initial_map : t 80 50 :=
  build_vertical_road 40 (HalfOpenRange.make 10 30 _) 
  (build_horizontal_road (HalfOpenRange.make 10 50 _) 25 (all_wall_map 80 50) _ _) _ _.
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
